{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, CPP, DeriveDataTypeable, DeriveFunctor #-}
module Plunger (plunge) where

import Language.Python.Version2
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation
import Control.Monad (join)
import Data.Maybe (catMaybes, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Debug.Trace
import System.Console.ANSI
import qualified System.Exit
import qualified Data.List
import Data.Data (Typeable)


data WarnType a = WarnUndefined String |
                  WarnMissingArgs [String] [FuncParam a] [(Maybe String, TypeDef a)] |
                  WarnExtraArg (Maybe String) (TypeDef a) |
                  WarnTypeMismatch (TypeDef a) (TypeDef a)
                  deriving Show

data Warning a =  Warning (WarnType a) a deriving Show

-- We assume that static members can only be luigi Parameters in a luigi task class. 
data ParsedClass a = ParsedClass {
                                    className:: String,
                                    classStatics::[FuncParam a],
                                    classMembers::[(String, TypeDef a)],
                                    classBody::[Statement a],
                                    classIsTask:: Bool,
                                    classConstructorType:: Maybe (TypeDef a)
                                   } deriving (Show)

--
-- Context is a list of objects available at some point in code, and their types.
--
--   "Closed" context means the list is exhaustive.
--   "Open" context means we know _some_ objects and their types,
--   but there may be more stuff we don't know about.
data CtxKind = CtxOpen | CtxClosed deriving Show
data Context a = Ctx CtxKind (Map.Map String (TypeDef a )) deriving Show

-- Formal function parameter with type and default value
data FuncParam  a = FuncParam {
     paramName::String
    ,paramType::TypeDef a
    ,paramDefault::Maybe (Expr a)
}  | Args | KwArgs deriving (Show)

data TypeDef a = AnyType |
               IntType |
               BoolType |
               StringType |
               ClassInstance (ParsedClass a) |
               ListType |
               SetType |
               ClassConstructor (ParsedClass a) |
               FuncType [FuncParam a] (TypeDef a) |
               TupleType [TypeDef a] |
               DictType (TypeDef a) (TypeDef a) deriving (Show, Typeable)


isSuperType :: TypeDef a -> TypeDef a -> Bool
isSuperType AnyType _ = True
isSuperType _ AnyType = True
isSuperType IntType IntType = True
isSuperType StringType StringType = True
isSuperType (ClassInstance a) (ClassInstance b) = (className a) == (className b)
isSuperType ListType ListType = True
isSuperType SetType SetType = True
isSuperType (ClassConstructor a) (ClassConstructor b) = (className a) == (className b)
isSuperType (FuncType aa ar) (FuncType ba br) = ((length aa) == (length ba)) && (isSuperType ar br)
isSuperType (TupleType a) (TupleType b) = (length a) == (length b)
isSuperType (DictType ka va) (DictType kb vb) = True    -- TODO: consider dictionary key/value types as well?
isSuperType _ _ = False


isTaskClass :: Argument t -> Bool
isTaskClass (ArgExpr (Dot (Var (Ident "luigi" _) _) (Ident "Task" _) _) _) = True
isTaskClass (ArgExpr (Dot (Var (Ident "luigi" _) _) (Ident "WrapperTask" _) _) _) = True
isTaskClass _ = False

-- Parse Luigi parameter (static member) as a class constructor parameter.
-- TODO: actually parse specific Luigi Parameter types (i.e. Parameter/DateParameter/IntParameter etc.)
parseLuigiArg :: Statement a -> Maybe (FuncParam a)
parseLuigiArg (Assign [(Var (Ident name _) _)] (Call (Dot (Var (Ident "luigi" _) _) (Ident ptype _) _) [] _) _) =
    Just $ (FuncParam name AnyType Nothing)

parseLuigiArg (Assign [(Var (Ident name _) _)] (Call (Dot (Var (Ident "luigi" _) _) (Ident ptype _) _) [ArgExpr a _] _) _) =
    Just $ (FuncParam name AnyType (Just a))

parseLuigiArg (Assign [(Var (Ident name _) _)] (Call (Dot (Var (Ident "luigi" _) _) (Ident ptype _) _) [ArgKeyword _ a _] _) _) =
    Just $ (FuncParam name AnyType (Just a))

parseLuigiArg _ = Nothing

-- Parse formal function parameter.
-- TODO: actually parse parameter type based on default value provided where possible.
parseParam :: Parameter t -> FuncParam t
parseParam (Param (Ident n _) _ Nothing _) = 
    FuncParam {paramName = n, paramDefault = Nothing, paramType = AnyType}

parseParam (Param (Ident n _) _ def _) = 
    FuncParam {paramName=n, paramDefault = def, paramType = AnyType}

parseMethodDef :: Statement a -> Maybe (String, TypeDef a)
parseMethodDef (Fun (Ident fname _) params _ _ _) =
    Just $ (fname, FuncType (tail $ map parseParam params) AnyType)

parseMethodDef _ = Nothing

getLuigiArgs :: [Statement a] -> [FuncParam a]
getLuigiArgs body = catMaybes $ map parseLuigiArg body

getMethods :: [Statement a] -> [(String, TypeDef a)]
getMethods body = catMaybes $ map parseMethodDef body

parseClass :: (Show t, Span t) => Statement t -> ParsedClass t
parseClass (Class (Ident x _) [arg] body _) =
    case isTaskClass arg of
        True -> ParsedClass x (getLuigiArgs body) (getMethods body) body True Nothing
        False -> ParsedClass x [] (getMethods body) body False (getConstructorType body)

parseClass (Class (Ident x _) [] body _) =
    ParsedClass x [] ((getMethods body) ++ (getClassMembers body)) body False (getConstructorType body)


getClassMembers ::  [Statement a] -> [(String, TypeDef a)]
getClassMembers body = concat $ catMaybes $ map parseConstructor body

getConstructorType ::   (Show t, Span t) =>  [Statement t] -> Maybe (TypeDef t)
getConstructorType body = case catMaybes $ map parseConstructorType body of
                            [x] -> Just x
                            [] -> Nothing

parseConstructorType :: (Show t, Span t) =>  Statement t -> Maybe (TypeDef t)
parseConstructorType (Fun (Ident "__init__" a1) (p:params) a2 body a3) =
    Just $ snd $ parseFunctionType (Fun (Ident "__init__" a1) params a2 body a3)

parseConstructorType _ = Nothing

parseConstructor :: Statement t -> Maybe [(String, TypeDef a)]
parseConstructor (Fun (Ident "__init__" _) (p:params) _ body _) =
    Just $ Map.toList $ Map.fromList $ parseConstructorStmts [] body


parseConstructor _ = Nothing

parseConstructorStmts ::
  [(String, TypeDef a)] -> [Statement t] -> [(String, TypeDef a)]
parseConstructorStmts res (x:xs) =
    case x of
        Assign vars expr _ -> 
            let parseV v = 
                    case v of 
                        Dot (Var (Ident "self" _) _) (Ident x _)  _ -> [(x, AnyType)]
                        _ -> []
            in
                (concatMap parseV vars) ++ res
        Try tbody _ else_ finally_ _ -> 
            parseConstructorStmts res (tbody ++ else_ ++ finally_)
        With withexprs wbody _ ->
            parseConstructorStmts res wbody
        For tgt gen fbody felse _ -> 
            parseConstructorStmts res (fbody ++ felse)
        Conditional guards else_ _ ->
                        let 
                            (_, bodies) = unzip $ guards
                        in
                            (parseConstructorStmts res ((concat bodies) ++ else_ ++ xs))

findClasses :: Module t -> [Statement t]
findClasses (Module sts) = filter (\x -> case x of
                                            Class _ _ _ _ -> True
                                            _ -> False) sts

findFunctions :: Module t -> [Statement t]
findFunctions (Module sts) = filter (\x -> case x of
                                            Fun _ _ _ _ _ -> True
                                            _ -> False) sts

findGlobals :: Module t -> [Statement t]
findGlobals (Module sts) = filter (\x -> case x of
                                            Assign _ _ _ -> True
                                            _ -> False) sts

findImports :: Module t -> [Statement t]
findImports (Module sts) = filter (\x -> case x of
                                            Import _ _ -> True
                                            FromImport _ _ _ -> True
                                            _ -> False) sts


parseVarName :: (Span t, Show t)=> Expr t -> String
parseVarName (Var (Ident x _) _) = x

-- Used to parse global vars.
-- FIXME: global vars are just assumed to always have string types
parseVarDef :: (Span t, Show t)=> Statement t -> [(String, TypeDef t)]
parseVarDef (Assign vars expr _) = map (\x -> ((parseVarName x), AnyType)) vars

parseFunctionType :: Span t=> Statement t -> (String, TypeDef t)
parseFunctionType (Fun (Ident fname _) params _ _ _) = (fname, FuncType (map parseParam params) AnyType)

-- Opportunistic imports support: if we have source code for the module
-- available, we recursively process it and find what is available in it.
-- If not, we just assume that the module object has AnyType.
parseImport :: (Show t, Span t) => (Map.Map String (Module t)) -> Statement t -> ([(String, TypeDef t)], [Warning t])
parseImport _ (Import items _) = (concatMap parseImportItem items, [])
parseImport modules (FromImport mod (FromItems items _) _) =
    case tryImport mod modules of
        Nothing -> (concatMap parseFromItem items, [])
        Just (ctx, warnings) ->
            let
                names = map fst $ concatMap parseFromItem items
                (td, w) = unzip $ map (\(FromItem i@(Ident _ _) _ _) -> checkDefined ctx i) items
            in
                (zip names td, concat $ w)

parseImport _ (FromImport mod (ImportEverything _) _) = ([], [])

parseImportItem :: Span t=> ImportItem t -> [(String, TypeDef t)]
parseImportItem (ImportItem modname (Just (Ident asname _)) _) = [(asname, AnyType)]
parseImportItem (ImportItem ((Ident x _):_) Nothing _) = [(x, AnyType)]

parseFromItem :: Span t=> FromItem t -> [(String, TypeDef t)]
parseFromItem (FromItem _ (Just (Ident asname _)) _) = [(asname, AnyType)]
parseFromItem (FromItem (Ident name _) Nothing _) = [(name, AnyType)]

tryImport :: (Show t, Span t) => ImportRelative t -> Map.Map String (Module t) -> Maybe (Context t, [Warning t])
tryImport (ImportRelative 0 (Just [Ident t _] ) _ ) importMap = 
    case Map.lookup t importMap of
        Just m -> 
                let 
                    (ns, w) = processModule importMap m
                in  Just (ns, w)
        Nothing ->
            -- trace ("can't import " ++ t ++ (show $ Map.keys importMap))
            Nothing

tryImport _ _ = Nothing

parseParamName :: Parameter t -> String
parseParamName (Param (Ident n _) _ _ _) = n

varsToCtx :: (Show t, Span t) => Context t -> [Expr t] -> Context t
varsToCtx ctx [] = ctx
varsToCtx ctx (v:vs) =
    case v of
        (Var _ _) ->
            let
                name = (parseVarName v)
                newc = addToCtx ctx [(name, AnyType)]
            in
                varsToCtx newc vs
        (Tuple es _) ->
            varsToCtx (varsToCtx ctx es) vs
        (Subscript _ _ _) -> varsToCtx ctx vs
        _ -> varsToCtx ctx vs

methodParamsToCtx ::
  Context t -> [Parameter t] -> ParsedClass t -> Context t
methodParamsToCtx ctx params this = 
    let
        selfp = (parseParamName (head params))
        newc = addToCtx ctx [(selfp, ClassInstance this)]
    in
        funParamsToCtx newc (tail params)

funParamsToCtx :: Context t -> [Parameter t] -> Context t
funParamsToCtx ctx [] = ctx 
funParamsToCtx ctx (p:params) = 
    let 
        pname = parseParamName p
        newc =  addToCtx ctx [(pname, AnyType)]
    in
        funParamsToCtx newc params

luigiMethods :: ParsedClass t -> [([Char], TypeDef t)]
luigiMethods _ =
    [("input", AnyType)]

funcParamsAsList :: [FuncParam a] -> [(String, TypeDef a)]
funcParamsAsList = map (\x -> (paramName x, paramType x)) 

classBuiltins = 
    [("__class__", AnyType)]

-- Convert a type (normally a class instance) to a context containing attributes.
contextFromType :: TypeDef t -> Context t
contextFromType AnyType = Ctx CtxOpen Map.empty
contextFromType (ClassInstance pc@ParsedClass{classIsTask=False}) = 
        let
             members = ((classMembers pc) ++ (funcParamsAsList $ classStatics pc)) ++ classBuiltins
        in
             makeCtx members
contextFromType (ClassInstance pc@ParsedClass{classIsTask=True}) = 
        let
             members = ((classMembers pc) ++ (funcParamsAsList $ classStatics pc)) ++ classBuiltins
        in
             makeCtx $ members ++ luigiMethods pc
contextFromType _ = (contextFromType AnyType)

checkMethods ::  (Span t, Show t) => Context t -> ParsedClass t -> [Warning t]
checkMethods ctx this =
    concatMap (checkMethod this ctx) (classBody this)

-- Check if identifier is defined in the context
checkDefined :: (Show t, Span t) => Context t -> Ident t -> (TypeDef t, [Warning t])
checkDefined ctx (Ident x loc) = 
    case ctxLookup ctx x of
        Nothing -> (AnyType, [Warning (WarnUndefined x) loc])
        Just t -> (t, [])

checkArg :: (Span t, Show t) => Context t -> t1 -> Argument t -> (Maybe String, (TypeDef t, [Warning t]))
checkArg ctx _ (ArgExpr e _) = (Nothing, checkExpr ctx e)
checkArg ctx _ (ArgKeyword (Ident n _) e _) = (Just n, checkExpr ctx e)
checkArg ctx _ (ArgVarArgsKeyword e _) = (Nothing, checkExpr ctx e)
checkArg ctx _ (ArgVarArgsPos e _) = (Nothing, checkExpr ctx e)

-- Check a list or dictionary comprehension.
-- TODO: currently we only check the "in" part of the comprehension
checkComprehension :: (Span t, Show t) => Context t -> Comprehension t -> (TypeDef t, [Warning t])
checkComprehension ctx (Comprehension (ComprehensionExpr e) (CompFor forexs inex iter _) _) =
    let
        varToCtx ctx (Var (Ident n _) _) = addToCtx ctx [(n, AnyType)]
        varToCtx ctx _ = ctx
        newc = foldl varToCtx ctx forexs
        (it, iw) = checkExpr ctx inex
        (et, ew) = checkExpr newc e
    in
        (ListType, iw ++ ew)

checkComprehension ctx (Comprehension (ComprehensionDict (DictMappingPair ek ev)) (CompFor forexs inex iter _) _) =
    let
        varToCtx ctx (Var (Ident n _) _) = addToCtx ctx [(n, AnyType)]
        varToCtx ctx _ = ctx
        newc = foldl varToCtx ctx forexs
        (it, iw) = checkExpr ctx inex
        (ekt, ekw) = checkExpr newc ek
        (evt, evw) = checkExpr newc ev
    in
        (ListType, iw ++ ekw ++ evw)

checkSlice :: (Span t, Show t) => Context t -> Slice t -> (TypeDef t, [Warning t])
checkSlice ctx (SliceProper l u s _) =
    let
        (_, w) = unzip $ catMaybes $ [(fmap (checkExpr ctx) l)
                                    , (fmap (checkExpr ctx) u)
                                    , (fmap (checkExpr ctx) (Control.Monad.join s))]
    in (AnyType, concat w)

-- Check if arguments passed to a function match its signature.
checkCallSignature :: (Span t, Show t) => [FuncParam t] -> [(Maybe String, TypeDef t)] -> t -> [Warning t]
checkCallSignature params cargs loc = 
    let
        -- check positional argument with atype
        checkCallSignature' f@(Args:restf) ((Nothing, atype):resta) =
             (checkCallSignature' f resta)

        -- named args
        checkCallSignature' (Args:_) []  =
            []

        checkCallSignature' (Args:restf) args =
             (checkCallSignature' restf args)

        -- check positional argument with atype
        checkCallSignature' (formal:restf) ((Nothing, atype):resta) =
            case isSuperType (paramType formal) atype of
                True -> checkCallSignature' restf resta
                False -> [Warning (WarnTypeMismatch atype (paramType formal)) loc] ++ (checkCallSignature' restf resta)

        -- named args
        checkCallSignature' f@(KwArgs:restf) ((Just name, atype):resta) =
             (checkCallSignature' f resta)

        -- named parameter
        checkCallSignature' formals ((Just name, atype):resta) =
            case Data.List.find pred formals of
                Just f -> case isSuperType (paramType f) atype of
                    True -> checkCallSignature' newf resta
                    False -> [Warning (WarnTypeMismatch atype (paramType f)) loc ] ++ (checkCallSignature' newf resta)
                Nothing ->
                    [Warning (WarnExtraArg (Just name) atype) loc] ++ (checkCallSignature' formals resta)
            where pred = (\x -> ((paramName x) == name))
                  newf = filter (not.pred) formals


        checkCallSignature' (KwArgs:_) []  =
            []

        -- find parameters that have no defaults and were not specified
        checkCallSignature' formals []  =
            case filter (isNothing.paramDefault) formals of
                [] -> []
                args -> [Warning (WarnMissingArgs (map paramName args) params cargs) loc]

        checkCallSignature' [] ((an, at):_)  =
            [Warning (WarnExtraArg an at) loc] 
    in
        checkCallSignature' params cargs

checkExpr :: (Span t, Show t) => Context t -> Expr t -> (TypeDef t, [Warning t])
checkExpr ctx expr =
    case expr of
        (Var i _) -> checkDefined ctx i
        (BinaryOp _op l r _) ->
                        let
                            (lt, lw) = checkExpr ctx l
                            (rt, rw) = checkExpr ctx r
                        in (lt, lw++rw)
        (UnaryOp _op a _) -> checkExpr ctx a
        (Call f a loc) -> let
                            -- check if function itself is defined
                            (ft, fw) =  checkExpr ctx f
                            -- check if argument expressions are okay
                            (an, atw) = unzip $ map (checkArg ctx f) a
                            (at, aw) = unzip atw
                            warnings = fw ++ (concat aw)
                        in
                            case ft of
                                FuncType fparams rt ->
                                    let
                                        -- check if function signature matches arguments and their types
                                        sw = checkCallSignature fparams (zip an at) loc
                                    in (rt, warnings ++ sw)
                                AnyType -> (AnyType, warnings)
                                _ -> trace (show loc) $ (AnyType, warnings)
        (Strings _ _) -> (StringType, [])
        (List exprs _) -> let
                            (_, w) = unzip $ map (checkExpr ctx) exprs
                          in (ListType, concat w)
        (Tuple exprs _) -> let
                            (tt, w) = unzip $ map (checkExpr ctx) exprs
                           in (TupleType tt, concat w)
        (Int _ _ _) -> (IntType, [])
        (Paren ex _) -> checkExpr ctx ex
        (Yield (Just (YieldExpr e)) _) -> checkExpr ctx e
        (ListComp c _) -> checkComprehension ctx c 
        (DictComp c _) -> checkComprehension ctx c 
        (Subscript ee se _) -> let
                                (et, ew) = checkExpr ctx ee
                                (st, sw) = checkExpr ctx se
                                warnings = ew ++ sw
                               in
                                case et of
                                    AnyType -> (AnyType, warnings)
                                    DictType _ v -> (v, warnings)
                                    _ -> (AnyType, warnings)
        (Dictionary pairs _) ->
                            let
                                exprs = concatMap (\(DictMappingPair e1 e2) -> [e1, e2]) pairs
                                (_, w) = unzip $ map (checkExpr ctx) exprs
                            in (DictType AnyType AnyType, concat w)
        (Dot dote (Ident attr loc) _) -> 
                        let (nt, w) = checkExpr ctx dote
                            (et, w2) = checkDefined (contextFromType nt) (Ident attr loc)
                        in (et, w ++ w2)

        (CondExpr true_ cond_ false_ _) ->
                        let (ct, cw) = checkExpr ctx cond_
                            (tt, tw) = checkExpr ctx true_
                            (ft, fw) = checkExpr ctx false_
                        in (tt, cw ++ tw ++ fw)

        (SlicedExpr slicee s _) ->
                        let (st, sw) = checkExpr ctx slicee
                            (it, iw) = unzip $ map (checkSlice ctx) s
                        in (AnyType, sw ++ (concat iw))

        x  -> trace ("cannot check expr " ++ (show x)) (AnyType, [])

checkStmts :: (Span t, Show t) => String -> Context t -> [Statement t] -> [Warning t]
checkStmts _ _ [] = []

checkStmts fname ctx (b:body) =
    case b of
        Pass _ -> checkStmts fname ctx body
        Return (Just expr) _ -> 
                        let
                            (_, w) = (checkExpr ctx expr)
                        in 
                            w ++ (checkStmts fname ctx body)
        Print _ exprs _ _ ->
                        let
                            (_, w) = unzip $ map (checkExpr ctx) exprs
                        in 
                            (concat w) ++ (checkStmts fname ctx body)
        Assign vars expr _ -> 
                        let
                            (_, warnings) = checkExpr ctx expr
                            newc = varsToCtx ctx vars
                        in
                            warnings ++ (checkStmts fname newc body)
        Try tbody _ else_ finally_ _ -> 
                        let 
                            bw = checkStmts fname ctx tbody
                            ew = checkStmts fname ctx else_
                            fw = checkStmts fname ctx finally_
                        in
                            bw ++ ew ++ fw ++ (checkStmts fname ctx body)
        Conditional guards else_ _ ->
                        let 
                            (conds, bodies) = unzip $ guards
                            (_, w) = unzip $ map (checkExpr ctx) conds
                        in
                            (concat w) ++ (checkStmts fname ctx ((concat bodies) ++ else_++body))
        -- NOTE: The scope for variables declared within the for loop
        -- is the loop body which is not how python actually works,
        -- but whatever, it is not a good coding practice anyway.
        For tgt gen fbody felse _ -> 
                        let 
                            (gt, warnings) = checkExpr ctx gen
                            newc = varsToCtx ctx tgt
                        in
                            warnings
                            ++ (checkStmts fname newc fbody)
                            ++ (checkStmts fname newc felse)
                            ++ (checkStmts fname newc body)
        StmtExpr e _ ->
                        let (_, warnings) = checkExpr ctx e
                        in warnings ++ (checkStmts fname ctx body)
        Assert exprs _ ->
                        let
                            (_, w) = unzip $ map (checkExpr ctx) exprs
                        in (concat w) ++ (checkStmts fname ctx body)
        With withexprs wbody _ ->
                        let
                            (exprs, maybevars) = unzip $ withexprs
                            (etypes, w) = unzip $ map (checkExpr ctx) exprs
                            withvars = foldl 
                                            (\res t -> case t of
                                                    ((Just (Var (Ident n _) _)), et) -> ((n, et):res)
                                                    _ -> res)
                                            [] (zip maybevars etypes)
                            newc = addToCtx ctx withvars
                            bodyw = checkStmts fname newc wbody
                        in
                            ((concat w) ++ bodyw) ++ (checkStmts fname ctx body)
        Continue _ ->  checkStmts fname ctx body
        Raise (RaiseV2 Nothing) _ ->  checkStmts fname ctx body
        Raise (RaiseV2 (Just (expr, Nothing))) _ -> 
                        let (_, warnings) = checkExpr ctx expr
                        in warnings ++ (checkStmts fname ctx body)
        x -> trace ("cannot check stmt " ++ (show x)) $ checkStmts fname ctx body

checkMethod ::  (Span t, Show t) => ParsedClass t -> Context t -> Statement t -> [Warning t]
checkMethod this ctx (Fun (Ident fname _) params _ body _) = 
             checkStmts fname (methodParamsToCtx ctx params this) body
checkMethod _ _ _ = [] 

makeCtx :: [(String, TypeDef t)] -> Context t
makeCtx vars = Ctx CtxClosed (Map.fromList $ map (\(n,t) -> (n, t)) vars) 

addToCtx :: Context t -> [(String, TypeDef t)] -> Context t
addToCtx (Ctx nt ctx) (x:rest) = let (n,t) = x in addToCtx (Ctx nt (Map.insert n t ctx)) rest
addToCtx n@(Ctx _ _) [] = n

ctxLookup :: Context t -> String -> Maybe (TypeDef t)
ctxLookup (Ctx nt ctx) name = 
        case Map.lookup name ctx of
            Just t -> Just t
            Nothing -> case nt of
                            CtxClosed -> Nothing
                            CtxOpen -> Just AnyType

-- Built-in functions/values. Need to add a lot more stuff here.
defaultCtx :: Context t
defaultCtx = makeCtx [("None", AnyType)
                     ,("True", BoolType)
                     ,("False", BoolType)
                     ,("Exception", FuncType [(FuncParam "" AnyType Nothing)] AnyType)
                     ,("dict", FuncType [Args, KwArgs] (DictType AnyType AnyType))
                     ,("sorted", FuncType [(FuncParam "" ListType Nothing)] ListType)
                     ,("len", FuncType [(FuncParam "" ListType Nothing)] IntType)
                     ,("zip", FuncType [(FuncParam "" ListType Nothing)] ListType)
                     ,("int", FuncType [(FuncParam "" AnyType Nothing)] IntType)
                     ,("list", FuncType [(FuncParam "" AnyType Nothing)] ListType)
                     ,("set", FuncType [(FuncParam "" AnyType Nothing)] SetType)
                     ,("str", FuncType [(FuncParam "" AnyType Nothing)] StringType)
                     ,("enumerate", FuncType [(FuncParam "" ListType Nothing)] ListType)
                     ,("map", FuncType [(FuncParam "" (FuncType [(FuncParam "" AnyType Nothing)] AnyType) Nothing), (FuncParam "" ListType Nothing)] ListType)
                     ,("range", FuncType [(FuncParam "" IntType Nothing)] ListType)
                     ,("open", FuncType [(FuncParam "" StringType Nothing), (FuncParam "" StringType Nothing)] AnyType)
                     ]

mkConstructorFunType :: ParsedClass t -> TypeDef  t
mkConstructorFunType c =
    case (classIsTask c) of
        True -> FuncType (classStatics c) (ClassInstance c)
        False -> case classConstructorType c of
                    Just t -> t
                    _ -> FuncType [] (ClassInstance c)

formatParam :: (Show a) => FuncParam  a -> String
formatParam FuncParam {paramName = name, paramType = p, paramDefault=Nothing} =  name ++ ":" ++ (show p)
formatParam FuncParam {paramName = name, paramType = p, paramDefault=Just d} =  name ++ ":" ++ (show p) ++ "="

showWarning :: (Show t, Span t) => Warning t -> IO ()
showWarning (Warning (WarnUndefined name) loc) = do
    putStr "Undefined "
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr name
    setSGR [Reset]
    putStr $ " at row " ++ (show $ startRow $ getSpan loc) ++ " column " ++ (show $ startCol $ getSpan loc)
    putStrLn ""

showWarning (Warning (WarnMissingArgs names func_params passed_args) loc) = do
    putStr ("Missing argument" ++ (if ((length names) > 1) then "s " else " "))
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr $ Data.List.intercalate ", " names
    setSGR [Reset]
    putStr $ " at row " ++ (show $ startRow $ getSpan loc) ++ " column " ++ (show $ startCol $ getSpan loc)
    putStrLn ""
    putStr $ "    Expected: " ++ (Data.List.intercalate ", " (map formatParam func_params))
    putStrLn ""

showWarning (Warning (WarnExtraArg maybename atype) loc) = do
    putStr "Extra argument "
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr $ (fromMaybe "" maybename) ++ (if isNothing maybename then "" else " ")
    setSGR [Reset]
--    putStr $ "of " ++ (show atype)
    putStr $ " at row " ++ (show $ startRow $ getSpan loc) ++ " column " ++ (show $ startCol $ getSpan loc)
    putStrLn ""

showWarning (Warning (WarnTypeMismatch a b) loc) = do
    putStr "Type mismatch "
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr $ show a
    setSGR [Reset]
    putStr " vs "
    setSGR [SetConsoleIntensity BoldIntensity]
    putStr $ show b
    setSGR [Reset]
    putStr $ " at row " ++ (show $ startRow $ getSpan loc) ++ " column " ++ (show $ startCol $ getSpan loc)
    putStrLn ""

processModule :: (Show t, Span t) => (Map.Map String (Module t)) -> Module t -> (Context t, [Warning t])
processModule modules sp = 
    let
        classes = map parseClass $ findClasses sp
        funs = map parseFunctionType $ findFunctions sp
        globals = concatMap parseVarDef $ findGlobals sp
        (imports, iwarnings) = unzip $ map (parseImport modules) $ findImports sp
        c = defaultCtx
        ctx = addToCtx c $ (concat imports) ++ funs ++ globals ++ (map (\x -> (className x, mkConstructorFunType x)) classes)
        warnings = concatMap (checkMethods ctx) $ classes
    in (ctx, (concat iwarnings) ++ warnings)

processAst ::
  (Show a, Show t1, Span t1) =>
  Map.Map String (Module t1) -> Either a (Module t1, t) -> IO ()
processAst modules (Right (sp, _)) = 
    case warnings of
        [] ->  do
                setSGR [SetColor Foreground Vivid Green]
                putStrLn "No warnings"
                setSGR [Reset]
                System.Exit.exitSuccess
        _ -> do
                setSGR [SetColor Foreground Vivid Red]
                putStrLn $ "=========== " ++ (show $ length warnings) ++ " WARNINGS ========"
                setSGR [Reset]
                putStrLn ""
                mapM_ (showWarning) warnings
                System.Exit.exitFailure
    where 
        (_, warnings) = processModule modules sp

processAst _ (Left err) = do
    putStrLn $ show err

plunge :: (Map.Map String String) -> String -> IO ()
plunge modules modname = 
    case Map.lookup modname modules of
        Just contents -> 
            let
                parsedModules = Map.mapMaybeWithKey (\k c ->
                                                case Language.Python.Version2.parseModule c (k ++ ".py") of 
                                                    (Right (sp, _)) -> Just sp
                                                    _ -> Nothing) modules
            in
                processAst parsedModules  $ Language.Python.Version2.parseModule contents (modname ++ ".py")
