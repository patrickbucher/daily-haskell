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

Run the REPL:

    $ cabal repl

## Modules

A minimalistic Cabal file looks as follows (`minimal.cabal`):

```cabal
cabal-version: 3.14
name:          minimal
version:       0.1.0.0
license:       NONE
executable minimal
    main-is:          Main.hs
    build-depends:    base
    hs-source-dirs:   app
    default-language: Haskell2010
```

An accompanying `Main.hs` file goes into the `app` folder of the project:

```haskell
module Main where

main = print "Hello, World!"
```

Code exported by some module can be imported to some other module.

The _Prelude_ is a special module that contains many functions and types, which are all imported implicitly.

Import an entire module (e.g. `Data.Char`):

```haskell
module Main where
import Data.Char
```

After which all symbols of `Data.Char` (e.g. `isPrint`) are available without further qualification.

Add the `Text` module as a dependency (`minimal.cabal`):

```cabal
--- ...
executable minimal
    main-is:          Main.hs
    build-depends:    base
                    , text
--- ...
```

Use a symbol (`length`) contained in both modules (`Prelude` and `Data.Text`):

```haskell
module Main where
import Data.Char
import Data.Text

main =
  let
    text = "Hello, World!"
  in
    print $ text <> ": " <> show (length text)
```

Fully qualify the call to `length` as `Prelude.length` to resolve the issue.

Use an alias name for an imported module to shorten the fully-qualified name:

```haskell
import Data.Text as T
```

Now `Text.length` can be referred to as `T.length`.

Multiple modules can be imported under the same alias:

```haskell
import Data.Text as T
import Data.Text.Encoding as T
```

Use _qualified_ imports to make sure every symbol has to be referred to using a qualfied name:

```haskell
import qualified Data.Char
import qualified Data.Text as T
```

Now `isPrint` has to be referred as `Data.Char.isPrint`, while the aliased `Data.Text` function `length` can be referred to as `T.length`.

Import only select symbols from a module, with or without alis:

```haskell
import Data.Char (isLower, isSpace)
import Data.Text as T (Text, length, pack)
```

The unaliased functions are available under their name (`isLower`, `isSpace`), while the aliased imports require the alias qualification (`T.Text`, `T.length`, `T.pack`).

Import an entire module without some specific symbols:

```haskell
import Data.Text hiding (length, filter)
```

This can be useful for providing own implementations:

```haskell
module Main where
import Prelude hiding (length)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

main = print $ length "Hello, World!"
```

Importing practices with pros and cons:

- standard imports
    - easiest
    - but produce name collisions
- qualified imports (without alias)
    - unambiguous
    - but very verbose
- qualified imports (with alias)
    - less verbose
    - but require additional naming conventions
- import lists
    - explicit and clear
    - but high maintenance

Personal preference: aliased import lists

## Creating Modules

TODO: p. 181
