# Learn You a Haskell for Great Good!

## Starting Out

List commands to be run upon GHCi startup in `~/.ghci`.

`head` returns the first, `last` the last element of a list. `tail` returns all but the first, `init` all but the last elements of a list.

`cycle` replicates a list, and `repeat` replicates a single element _ad infinitum_; `replicate` repeats a single element for a given amount of times.

## Believe the Type

`compare` returns `GT` (greater than), `LT` (less than), or `EQ` (equal to):

    > 5 `compare` 3
    GT
    > 2 `compare` 4
    LT
    > 3 `compare` 3
    EQ

Instances of te `Enum` type class support the `succ` (successor) and `pred` (predecessor) functions:

    > succ 'a'
    'b'
    > pred 17
    16

`minBound` and `maxBound` are polymorphic constants that denote lower and upper limits of a type:

    > minBound :: Char
    '\NUL'
    > maxBound :: Int
    9223372036854775807

The `Floating` type class has the instances `Float` (single precision) and `Double` (double precision); `Integral` has `Int` (fixed size) and `Integer` (arbitrary size). An `Integral` can be converted to a `Floating` type using `fromIntegral`:

    > fromIntegral :: Double 17
    17.0

## Syntax in Functions

`error` expectes a string as an argument and generates a runtime error that crashes the program:

    > error "nothing left to do"
    *** Exception: nothing left to do

Use an _as-pattern_ to keep access to a matched value in its entirety:

```haskell
describe :: Show a => [a] -> String
describe all@(x:_) =
  "list of length " ++ show (length all) ++ " with head " ++ show x
```

The definitions within a `where` clause are available in all guarded arms of a function clause:

```haskell
judgeBMI :: Double -> Double -> String
judgeBMI weight height
  | bmi < 20 = "underweight"
  | bmi < 25 = "normal weight"
  | bmi < 30 = "overweight"
  | otherwise = "obese"
  where
    bmi = weight / height ^ 2
```

Bindings introduced within `let` are only accessible within the corresponding `in` expression:

```haskell
pythagoras :: Double -> Double -> String
pythagoras a b =
  let c = sqrt (a ^ 2 + b ^ 2)
   in show a ++ "^2+" ++ show b ++ "^2=" ++ show c ++ "^2"
```

Unlike `where`, `let`/`in` is an expression and returns a value:

    > 4 * (let q = 3 in 4 / q)
    5.333333333333333

Multiple definitions can be separated by semicolons:

    > 4 * (let p = 4; q = 3 in p / q)
    5.333333333333333

Local functions can be introduced using `let`/`in`:

    > let twice n = 2 * n in [twice 3, twice 4, twice 5]
    [6,8,10]

`let`/`in` is also allowed in list comprehensions:

```haskell
triplets :: Int -> [(Int, Int, Int)]
triplets n =
  [ (a, b, c)
  | a <- [1 .. n]
  , b <- [a .. n]
  , c <- [b .. n]
  , let a' = a ^ 2
  , let b' = b ^ 2
  , let c' = c ^ 2
  , a' + b' == c'
  ]
```

Bindings introduced with `let` in GHCi are available for the entire session and can be overwritten:

    > let a = 1
    > let a = 3
    > let b = 2
    > let b = 4
    > let c = a^2 + b^2
    > c
    25

## Hello, Recursion!

-

## Higher-Order Functions

`scanl` and `scanr` work like `foldl` and `foldr` but return a list of all intermediate accumulator states, including the initial one:

    > scanl (+) 0 [1..4]
    [0,1,3,6,10]
    > scanr (+) 0 [1..4]
    [10,9,7,4,0]

The resulting list is one element longer then the processed one, the additional element being the initial accumulator value.

There are also `scanl1` and `scanr1` variants, which assume the first or last element, respectively, as the initial accumulator value:

    > scanl1 (+) [1..4]
    [1,3,6,10]
    > scanr1 (+) [1..4]
    [10,9,7,4]

Now the list being returned and the one processed have the same length.

The function application operator `$` has the lowest precedence and is right-associative. Its use makes allows to remove the surrounding parentheses from the expression on the right-hand side:

    > sum (map (^2) [0..4])
    30
    > sum $ map (^2) [0..4]
    30

It can be used to apply functions to values:

    > map ($ 4) [(1+),(2^),(3*),(4-)]
    [5,16,12,0]

## Modules

A Haskell _module_ is a file containing functions, some of which are exported. Functions from other modules, e.g. `Data.Char` from the standard library, can be imported using `import`:

```haskell
import Data.Char
```

Use [haskell.org/hoogle](https://hoogle.haskell.org/) to find modules that contain functions for your needs.

In GHCi, use the `:m +` (short for `:module +`) command to import modules:

    > :module + Data.Char
    > :m + Data.List

Multiple modules can be imported on one line:

    > :m + Data.List Data.Char

Import individual functions as follows:

```haskell
import Data.Char (digitToInt, isDigit)
```

Import all but certain functions with `hiding`:

```haskell
import Data.List hiding (sort)
```

Use qualified imports to avoid namespace clashes:

```haskell
import qualified Data.Char as C
import qualified Data.List as List

toLargestNumber :: String -> Int
toLargestNumber =
  read
    . map C.intToDigit
    . reverse
    . List.sort
    . map C.digitToInt
    . filter C.isDigit
```

Use the `foldl'` (or `foldl1'`) alternatives from `Data.List` for _strict folds_ that do not defer computations:

    > foldl (+) 0 (replicate 100000000 1)
    *** Exception: stack overflow
    > import Data.List (foldl')
    > foldl' (+) 0 (replicate 100000000 1)
    100000000

The `Data.Map` module provides _maps_ to store key-value pairs:

```haskell
import qualified Data.Map as Map
```

Create a map from a list of key-value tuples:

    > let capitals = Map.fromList [("Switzerland","Bern"),("Germany","Berlin"),("USA","Washington D.C.")]
    > capitals
    fromList [("Germany","Berlin"),("Switzerland","Bern"),("USA","Washington D.C.")]
    > Map.size capitals
    3

Use `lookup` to find and `insert` to add or overwrite values:

    > Map.lookup "Germany" capitals
    Just "Berlin"
    > Map.insert "USA" "Tel Aviv" capitals
    > updated
    fromList [("Germany","Berlin"),("Switzerland","Bern"),("USA","Tel Aviv")] 

When possibly importing duplicates, use `fromListWith` with a function that deals with duplicates:

    > Map.fromListWith max [("Alice",7.44),("Bob",8.21),("Alice",7.99)]
    fromList [("Alice",7.99),("Bob",8.21)]

Declare your own module (e.g. `Geometry`) in its own file (e.g. `Geometry.hs`):

```haskell
module Geometry
  ( circleCircumference
  , circleArea
  , squareCircumference
  , squareArea
  ) where

circleCircumference :: Double -> Double
circleCircumference r = 2 * r * pi

circleArea :: Double -> Double
circleArea r = r * pi ^ 2

squareCircumference :: Double -> Double
squareCircumference s = 4 * s

squareArea :: Double -> Double
squareArea s = s ^ 2
```

For hiearchical modules, create a folder for the top-level module (e.g. `Geometry`) containing the files for the lower-level modules (e.g. `Circle.hs` and `Square.hs`).

`Geometry/Circle.hs`:

```haskell
module Geometry.Circle
  ( circleCircumference
  , circleArea
  ) where

circleCircumference :: Double -> Double
circleCircumference r = 2 * r * pi

circleArea :: Double -> Double
circleArea r = r * pi ^ 2
```

`Geometry/Square.hs`:

```haskell
module Geometry.Square
  ( squareCircumference
  , squareArea
  ) where

squareCircumference :: Double -> Double
squareCircumference s = 4 * s

squareArea :: Double -> Double
squareArea s = s ^ 2
```

Use those modules in GHCi:

    $ ghci Geometry/*.hs
    > import qualified Geometry.Circle as C
    > import qualified Geometry.Square as S
    > C.circleCircumference 5
    31.41592653589793
    > S.squareArea 4
    16.0

## Making Our Own Types and Type Classes

Export the value constructors for export within parentheses. Use `(..)` to export all defined constructors:

```haskell
module RockPaperScissors
  ( Player(First, Second)
  , Move(..)
  , Outcome(..)
  ) where

data Player
  = First
  | Second

data Move
  = Rock
  | Paper
  | Scissors

data Outcome
  = Win Player
  | Draw
```

Use record syntax to name the fields of a new data type:

```haskell
data Athlete = Athlete
  { fullName :: String
  , age :: Int
  , country :: String
  , winRatio :: Float
  } deriving (Show)

joe :: Athlete
joe = Athlete "Joe Doe" 26 "USA" 0.37

jay :: Athlete
jay = Athlete {fullName = "Jay Day", winRatio = 0.68, age = 39, country = "GB"}
```

Use type constraints with functions rather than `Data` declarations to avoid unnecessary rigid constraints.

Derive `Enum` for things with predecessors (`pred`) and successors (`succ`). Derive `Bounded` for things with a lower (`minBound`) and an upper (`maxBound`) bound:

```haskell
data Day
  = Mo
  | Tu
  | We
  | Th
  | Fr
  | Sa
  | Su
  deriving (Bounded, Enum, Show)
```

    > (pred Sa, succ Sa)
    (Fr,Su)
    > [minBound..maxBound] :: [Day]
    [Mo,Tu,We,Th,Fr,Sa,Su]

This only works for _nullary_ declarations, i.e. for constructors that don't require additional fields:

```haskell
data WorkDay
  = Monday Bool
  | Tuesday Bool
  | Wednesday Bool
  | Thursday Bool
  | Friday Bool
  | Saturday Bool
  | Sunday Bool
  deriving (Bounded, Enum, Show)
```

    error:
        Can't make a derived instance of ‘Enum WorkDay’
        …
