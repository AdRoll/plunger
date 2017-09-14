module Main where

import System.Environment (getArgs)
import Plunger (plunge)
import System.FilePath.Posix (takeBaseName)
import qualified Data.Map.Strict as Map

readModules :: [String] -> IO (Map.Map String String)
readModules filenames = do
    sources <- mapM (\x -> readFile x) filenames
    return $ Map.fromList $ zip (map takeBaseName filenames) sources

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> putStrLn "Usage: plunger path/to/module.py"
                filenames -> do
                        modules <- readModules filenames
                        modname <- return $ takeBaseName $ head filenames
                        plunge modules modname
