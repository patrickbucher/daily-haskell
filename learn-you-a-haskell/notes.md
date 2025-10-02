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

