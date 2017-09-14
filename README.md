# plunger

Plunger is a python linter, with bult-in [Luigi](https://github.com/spotify/luigi) support.

## Rationale

Existing Python linters and type checkers ([pylint](https://www.pylint.org/), [mypy](http://mypy-lang.org/)) are great, but have their limitations: you can only get so far when dealing with Python code that uses run-time metaprogramming magic. Unfortunately Luigi relies on this quite a bit, that makes generic linters less useful.

For example, they can't effectively detect errors related to Luigi task parameters when they are used as member variables or arguments to the constructor. `pylint` can detect undefined variables only when they are normal local variables, not class members -- most of the time it has no idea if `self.foo` exists or not.

## Solution

These limitations are mostly due to highly dynamic nature of Python, you can't really get around this in a generic way. Plunger's answer is to just give up on that and treat Luigi tasks specially. Plunger only supports a certain well behaved subset of Python. Features like `import *` or inheritance (with the exception of inheriting from luigi.Task) are not supported, but you shouldnt use them anyway.

## Installation

Get [Stack](https://github.com/commercialhaskell/stack/releases)
```
stack build plunger
stack install plunger
```

You should get `plunger` executable under `~/.local/bin`. 

## Usage

```
plunger path/to/module.py [path/to/imported.py ...]
```

Where first path is to the module to be checked, and the are paths to imported modules to be checked as well. 

`plunger` has only limited support for imports, so it will only process the imported modules that you specify on the command line, the rest will be ignored. Only `from X import Y` imports are supported at the moment.

To illustrate what this means in practice, consider two python modules:

##### otherjobs.py
```python
import luigi

class TestTask(luigi.Task):
    foo = luigi.Parameter()
    bar = luigi.Parameter()

    def run(self):
        pass
```


##### myjobs.py
```python
import luigi
from otherjobs import TestTask

class OtherTestTask(luigi.Task):

    def requires(self):
        return TestTask(foo="a")  # missing argument for "bar" here 

    def run(self):
        pass
```

You'll get no warnings if you run `plunger` on **myjobs.py** only:
```shell
$ plunger myjobs.py
No warnings
```

Since `plunger` does not automatically process all imports, it has no idea what the signature of `TestTask` is, and cannot detect the error (missing parameter).

But if you specify path to the other module on the command line, it will parse it as well and will be able to verify the invocation of `TestTask` constructor:
```shell
plunger myjobs.py otherjobs.py
=========== 1 WARNINGS ========

Missing argument bar at row 8 column 16
    Expected: foo:AnyType, bar:AnyType
```

# Examples of errors detected by Plunger

#### Undefined class members

```python
from __future__ import print_function
import luigi

class TestTask(luigi.Task):
    foo = luigi.Parameter()
    bar = luigi.Parameter()

    def run(self):
        print(self.y)
```

Result:

```
=========== 1 WARNINGS ========

Undefined y at row 10 column 20
```

#### Missing task constructor parameters

```python
from __future__ import print_function
import luigi

class TestTask(luigi.Task):
    foo = luigi.Parameter()
    bar = luigi.Parameter()

    def run(self):
        pass

class OtherTestTask(luigi.Task):
    def requires(self):
        return TestTask(foo="a") 

    def run(self):
        pass
```

Result:

```
=========== 1 WARNINGS ========

Missing argument bar at row 16 column 16
    Expected: foo:AnyType, bar:AnyType
```

