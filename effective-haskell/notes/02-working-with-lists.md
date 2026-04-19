# Working with Lists

Strings are lists:

```ghci
λ ['h','e','l','l','o'] == "hello"
```

Thus, the concatenation operator `<>` can be used for any kind of list:

```ghci
λ [1,2,3] <> [4..9]
[1,2,3,4,5,6,7,8,9]
```

The n-th element of a list can be accessed using the `!!` operator:

```ghci
λ "hello" !! 4
'o'
λ [10,20..100] !! 5
60
```

Prepend ("cons") an element to a list:

```ghci
λ 0 : [1..3]
[0,1,2,3]
λ 0 : 1 : 2 : 3 : [4..9]
[0,1,2,3,4,5,6,7,8,9]
```

Access the _head_ (first element) and _tail_ (remainder) of a list:

```ghci
λ head [0..4]
0
λ tail [0..4]
[1,2,3,4]
λ tail [1]
[]
λ head []
ERROR
λ tail []
ERROR
```

Both `head` and `tail` are _partial_ functions, i.e. they are not defined for every input, e.g. `head` and `tail` on an empty list.

Check if a list is empty:

```ghci
λ null [1,2,3]
False
λ null []
True
```

Build up a list recursively:

```ghci
λ countdown n = if n == 0 then [] else n : countdown (n - 1)
λ countdown 10
[10,9,8,7,6,5,4,3,2,1]
```

## Folding

Re-implement `foldl` and `foldr`:

```haskell
foldLeft f acc xs =
  if null xs
  then acc
  else foldLeft f (f acc (head xs)) (tail xs)

foldRight f acc xs =
  if null xs
  then acc
  else f (head xs) $ foldRight f acc (tail xs)
```

- A left fold calculates the next carry value (`acc`) and passes it on to the recursive call.
- A right fold passes the original carry value (`acc`) to the recursive call.

`foldLeft` is tail-recursive, `foldRight` builds up a recursive chain of computation.

Subtract using `foldl` and `foldr`:

```ghci
λ foldl (-) 10 [7,3] -- ((10 - 7) - 3)
0
λ foldr (-) 3 [7,10] -- (7 - (10 - 3))
0
λ foldl (-) 0 [7,3,1] -- (((0 - 7) - 3) - 1)
-11
λ foldr (-) 0 [7,3,1] -- (7 - (3 - (1 - 0)))
5
```

## Transformations

Apply a function to every element using `map`:

```ghci
λ map (+1) [0..9]
[1,2,3,4,5,6,7,8,9,10]
```

Apply a value to a list of functions:

```ghci
λ map ($ 10) [(+1), (*2), (1/)]
[11.0,20.0,0.1]
```

Write your own `map` function:

```haskell
map' f xs =
  if null xs
  then []
  else f (head xs) : map' f (tail xs)
```

Apply a predicate function to every element to _filter_ the list:

```ghci
λ filter (\x -> x `rem` 2 == 0) [0..9]
[0,2,4,6,8]
```

Build a list using a _list comprehension_:

```ghci
λ [2 * x | x <- [0..9]]
[0,2,4,6,8,10,12,14,16,18]
```

To the left of the `|` operator goes the expression that ends up in the list, to its right is the generator expression that comes up with the individual values.

Build a list with an additional predicate:

```ghci
λ [2 * x | x <- [1..9], even x]
[4,8,12,16]
```

Build a list with multiple generator expressions (building the carthesian product):

```ghci
λ [(x,y) | x <- ["a","b","c"], y <- [1,2]]
[("a",1),("a",2),("b",1),("b",2),("c",1),("c",2)]
```

Think of the first generator expression as the outer and the second as the inner "loop" to predict the order of elements.

Combine the elements of two lists pair-wise:

```ghci
λ zip ["a","b","c"] [1..]
[("a",1),("b",2),("c",3)]
```

The result will be as long as the shorter of the two input lists.

## Pattern Matching

Match function arguments against specific values:

```haskell
fib 1 = 1
fib 2 = 1
fib n = fib (n - 2) + fib (n - 1)
```

The specific cases must be listed above the general case; a variable will match any expression.

Match tuples and lists:

```haskell
describePoint (0, 0) = "origin"
describePoint (0, y) = "on x-axis; y = " <> show y
describePoint (x, 0) = "x = " <> show x <> "; on y-axis"
describePoint (x, y) = "P(" <> show x <> "," <> show y <> ")"

addUp [] = 0
addUp (h:t) = h + addUp t
```

Keep a matched value in its entirety using the `@` operator:

```haskell
describePoint p@(0,0) = show p <> "is the origin"
describePoint p@(0,y) = show p <> "is on x-axis with y=" <> show y
describePoint p@(x,0) = show p <> "is on y-axis with x=" <> show x
describePoint p@(x,y) =
  show p <> "is on no axis with x=" <> show x <> ", y=" <> show y
```

Prefix or replace a variable with `_` to match everything and to discard the value.

Match using `case`/`of`:

```haskell
prefix x =
  case round (logBase 10 x) of
    0 -> ""
    1 -> "deka"
    2 -> "hekta"
    3 -> "kilo"
    _ -> "?"
```

Use the `-Wincomplete-patterns` flag to detect non-exhaustive patterns (works for both the compiler `ghc` and the interpreter `ghci`). In GHCi, the option can be toggled interactively:

```ghci
λ :set -Wincomplete-patterns
λ :set -Wno-incomplete-patterns
```
