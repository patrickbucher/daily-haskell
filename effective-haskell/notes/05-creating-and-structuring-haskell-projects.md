# Creating And Structuring Haskell Projects

## Package Manager

Update the Cabal package cache:

    $ cabal update

Make sure to include `$HOME/.cabal/bin` in your `$PATH`.

Install `stylish-haskell` (a formatter):

    $ cabal install stylish-haskell

Install `text` (a library):

    $ cabal install --lib text

## Project Manager

Create a new project interactively:

    $ mkdir sandbox
    $ cd sandbox
    $ cabal init --interactive

Pick the _Library and Executable_ option, so that most of the application code ends up in a separate library, with a small executable, which facilitates testing.

Pick the most recent optoin for the Cabal specification, e.g. 3.14.

The application source code goes to the `app` folder, the library source code into the `src` folder.

A file `sandbox.cabal`, containing all the selected options, has been generated.

The Cabal file format mostly consists of (nested) key-value pairs and comments starting with `--` going to the end of the line.

Version specifications (`build-depends`) can be specified using the comparison (`<`, `<=`, `==`, `>`, `>=`) and logical operators  (`&&`, `||`), e.g.:

```cabal
build-depends: base >=4.13.0.0 && <4.14
```

Alternatively, use the `^` operator, which restricts version numbers to compatible variants:

```cabal
build-depends: base ^>=4.13.0.0
```

Version numbers are indicated using the Haskell _Package Version Policy_ (PVP) consisting of four numbers: `generation.major.minor.patch`.

The `ghc-options` allows to pass options to the compiler, e.g.

- Optimizations
    - `-O0`: no optimizations
    - `-O1`: some optimizations
    - `-O2`: more optimizations (slower compilation)
- Warnings/Errors
    - `-Wall`: common warnings
    - `-Weverything`: every known warning
    - `-Werror`: turn warnings into errors (refuse compilation)
- Profiling
    - `-prof`: enable profiling

Configure the Cabal project to gather complete profiling information:

    $ cabal configure --enable-profiling

Build the the entire project (library and executable):

    $ cabal build

Build specific targets (library _or_ executable):

    $ cabal build lib:sandbox
    $ cabal build exe:sandbox

Run the executable:

    $ cabal exec sandbox

Build and execute using a single command:

    $ cabal run sandbox

TODO: p. 167 "Using Code from Other Modules"

