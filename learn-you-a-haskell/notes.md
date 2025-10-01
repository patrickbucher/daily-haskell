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
    
