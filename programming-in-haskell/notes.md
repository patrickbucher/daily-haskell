# Introduction

The function `double` takes a number `x` as its argument and produces the result `x + x`:

```haskell
double :: Num a => a -> a
double x = x + x
```

Applied to the argument `3`, the function is evaluated as follows:

```
double 3
= 3 + 3
= 6
```

A nested application with the argument `x` is evaluated as follows:

```
double (double 2)
= double (2 + 2)
= double 4
= 4 + 4
= 8
```

Starting with the outer application of `double`, the evaluation would look as follows:

```
double (double 2)
double 2 + double 2
= (2 + 2) + double 2
= 4 + double 2
= 4 + (2 + 2)
= 4 + 4
= 8
```

> _Functional programming_ is a style of programming in which the basic method of computation is the application of functions to arguments. A _functional programming language_ is a language that supports this style of programming.

This Java program sums up the numbers from 1 to `n`:

```java
int total = 0;
for (int count = 1; count < n; count++)
    total = total + count;
```

This program operates by changing stored values—the variables `count` and `total`, which is called _imperative programming_.

The same program can be expressed as follows using the Haskell standard library function `sum`:

```haskell
sum [1..n]
```

Which can be implemented as follows:

```haskell
sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + sum ns
```

- For any type of numbers `a`, the function maps a list of numbers to a single number.
- The sum of an empty list is zero.
- A list consisting of a head `n` and a remainder `ns` has the sum of `n` plus the sum of the remainder `ns`.

The Quicksort algorithm can be implemented as follows:

```haskell
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
```

- For any ordered type, the function maps a list of this type to a list of the same type.
- An empty list is considered sorted.
- A non-empty list is sorted by recursive calls to `qsort` of the smaller and larger numbers than `x`. Those are built using list comprehensions in the `where` clause and concatenated using the `++` operator.

The `sqn` function takes a list of input/output actions, performs them, and returns a list of their resulting values:

```haskell
seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do
  x <- act
  xs <- seqn acts
  return (x : xs)
```

- For a list of `IO` actions that produce a value of type `a`, the function returns a single `IO` action of a list of type `a`.
- An empty list is considered processed.
- A non-empty list is processed by performing the first action, storing its return values in the variable `x`, passing the remainder of the actions to a recursive `seqn` call, and returns the concatenated results.

The type definition can be generalised to any operation involving side effects:

```haskell
seqn :: Monad m => [m a] -> m [a]
```

Haskell's main benefits are:

- Concise programs due to its terse syntax and high-level nature.
- Powerful type system with strong type inference.
- List comprehensions to construct new lists based on existing ones.
- Recursive functions with pattern matching and guards rather than relying on loops.
- Higher-order functions for function composition, which allows to define domain-specific languages.
- Effectful functions to write pure functions with side-effects using monads and applicatives.
- Generic functions that support various different data types.
- Lazy evaluation to avoid unneccessary computations.
- Equational reasoning to effectively reason about, write, and transform programs.

Haskell is based on historical developments:

- 1930s: Lambda Calculus (Alonso Church), a mathematical theory for functions
- 1950s: Lisp (John McCarthy), arguably the first functional programming language
- 1960s: ISWIM (Peter Landin), the first _pure_ functional programming language
- 1970s: FP (John Backus), a functional programming language with higher-order functions
- 1970s: ML (Robin Milner), a modern functional programming language with polymorphic types and type inference
- 1970s/1980s: Miranda (David Turner), a lazy functional programming language
- 1987: initiation of the Haskell programming language
- 1990s: type classes and monads (Phil Wadler) for overloading and effects
- 2003: publication of the Haskell Report (first stable version of the language)
- 2010: publication of the revised Haskell Report

# First Steps

The Glasgow Haskell Compiler (GHC) features an interactive mode (GHCi) that can be started using the `ghci` command. The interactive prompt `>` can be used to evaluate numerical expressions:

```ghci
> sqrt (3^2 + 4^2)
5.0
```

Besides mathematical operators, the _standard prelude_ provides various functions that operate on lists:

- `head`: get the first element of a non-empty list
- `tail`: get all but the first element of a list
- `!!`: get the `n`-th element of a list (zero-based index)
- `take`: get the first `n` elements of a list
- `drop`: get all but the first `n` elements of a list
- `length`: get the length of a list
- `sum`: calculate the sum of the list's elements
- `product`: calculate the product of the list's elements
- `++`: append two lists
- `reverse`: reverse the order of a list

Examples:

```ghci
> head [1, 2, 3]
1
> tail [1, 2, 3]
[2, 3]
> [1, 2, 3] !! 2
3
> take 2 [1, 2, 3]
[1, 2]
> drop 2 [1, 2, 3]
[3]
> length [1, 2, 3]
3
> sum [1, 2, 3, 4]
10
> product [1, 2, 3, 4]
24
> [1, 2, 3] ++ [4, 5]
[1, 2, 3, 4, 5]
> reverse [1, 2, 3]
[3, 2, 1]
```

Function application has the highest operator precedence and does not require explicit parentheses, i.e. `f x` and `f(x)` are equivalent. A function expecting two arguments can be written as a infix operator within backticks:

```ghci
> div 10 3
3
> 10 `div` 3
3
```

By convention, Haskell scripts are stored in files with the extension `.hs`. Such a script (e.g. `test.hs`) can be loaded into `hgci` as follows:

```bash
$ ghci test.hs
```

After modifying the script, it can be reloaded using the `:reload` command. Commands can be abbreviated using their first letter, e.g. `r` for `reload`. Some common commands:

- `:load PATH`: load a script under its path
- `:reload`: reload the current script
- `:set editor NAME`: define the editor to be used for the `:edit` command
- `:edit PATH`: edit the script under its path using the defined `editor`
- `:type EXPR`: determine the type of an expression
- `:?`: show all commands
- `quit`: quit GHCi

Function names start with a lower-case letter, followed by letters (upper- and lowercase), digits, underscores, and forward single quotes. The suffix `s` denotes plurals, e.g. `xs` stands for a list, `ns` for a list of numbers, and `css` for a list of lists of characters.

Indentation matters; definitions within a `where` block must be aligned. Grouping of expressions can be achieved using `{` and `}`. Spaces should be used rather than tabs. There are single-line comments starting with `--` and multi-line comments within `{-` and `-}`.

# Types and classes

The notation `v :: T` means that value `v` has type `T`. More general, `e :: T` means that the yet unevaluated expression `e` produces a value of type `T` upon evaluation. The determination of the type of an expression is handled by a process called _type inference_ that works as follows: If a function `f` that maps arguments of type `A` to results of type `B` (`f :: A -> B`), and `e` is an expression of type `A` (`e :: A`), then the application of `f` has type `B` (`f e :: B`). Since type inference happens before evaluation, Haskell programs are said to be _type safe_.

Haskell supports various basic types:

- `Bool`: the logical values `True` and `False`
- `Char`: single Unicode characters enclosed in single quotes, e.g. `'a'`
- `String`: sequences of characters enclosed in double quotes, e.g. `"abc"`
- `Int`: fixed-precision integers from `-2^63` to `2^63-1`
- `Integer`: arbitrary-precision integers without lower and upper limits
- `Float`: single-precision floating-point numbers
- `Double`: double-precision floating-point numbers

A numeric expression can match different data types.

A list is a sequence of elements written comma-separated within square brackets (`[]`) of the same type with potential unlimited size.

A tuple is a finite sequence of components written comma-separated within parentheses (`()`) of possibly different types and limited size. The number of components is called the tuple's _arity_. The empty tuple `()` is allowed, but not tuples with the size of one for syntactic reasons.

Lists and tuples can be combined to tuples of lists, lists of tuples, etc.

Functions can expect multiple arguments as tuples (`hypot`) or one by one using _currying_ (`hypot'`):

```haskell
hypot :: (Float, Float) -> Float
hypot (a, b) = sqrt (a ^ 2 + b ^ 2)

hypot' :: Float -> (Float -> Float)
hypot' a b = sqrt (a ^ 2 + b ^ 2)
```

- The `hypot` function expects a tuple of arity 2 and returns a `Float` value.
- The `hypot'` function expects a single `Float` value and returns another function that expects another `Float` value and returns a `Float` value.

The functions are called as follows:

```ghci
> hypot (3.0, 4.0)
5.0
> ((hypot' 3.0) 4.0)
5.0
```

Since the operator `->` in type definitions associates to the right, and function application associates to the left, the type definition and invocation of `hypot'` can be simplified as follows (i.e. the parentheses can be left away):

```haskell
hypot' :: Float -> Float -> Float
hypot' 3.0 4.0
```

_Type variables_ begin with a small letter and can stand for different types, e.g. `length :: [a] -> Int` for the `length` function that works on any type denoted by the variable `a` rather than a concrete type.

_Class constraints_ are written as `C a`, where `C` is the name of a type class and `a` the type variable restricted by the class. Type `a` is said to be an _instance_ of class `C`. A type containing at least one constraint is called _overloaded_, e.g. the operator `+` (defined as the function `(+)`) is defined as:

```haskell
(+) :: Num a => a -> a -> a
```

Which means that the type variable `a` must stand for any type of class `Num` (i.e. any numeric type).

Haskell supports many type classes that support different operatiosn:

- `Eq`: equality types (all basic types, tuples, list—but not functions)
    - `(==) :: a -> a -> Bool`: equality
    - `(/=) :: a -> a -> Bool`: inequality
- `Ord`: types of `Eq` that also can be ordered and compared
    - `(<) :: a -> a -> Bool`: lesser than
    - `(<=) :: a -> a -> Bool`: lesser than or equal to
    - `(>) :: a -> a -> Bool`: greater than
    - `(>=) :: a -> a -> Bool`: greater than or equal to
    - `min :: a -> a -> a`: the smaller of the two values
    - `max :: a -> a -> a`: the smaller of the two values
- `Show`: showable types, i.e. values that can be represented as a `String`
    - `show :: a -> String`: show as a `String`
- `Read`: readable types, i.e. `String` values that can be parsed to other types
    - `read :: String -> a`: parse from a `String`
- `Num`: numeric types
    - `(+) :: a -> a -> a`: addition
    - `(-) :: a -> a -> a`: subtraction
    - `(*) :: a -> a -> a`: multiplication
    - `(negate) :: a -> a`: change of sign
    - `(abs) :: a -> a`: absolute value
    - `(signum) :: a -> a`: determine sign
- `Integral`: integer types (`Int` and `Integer`)
    - `div :: a -> a -> a`: integer division
    - `mod :: a -> a -> a`: remainder of integer division
- `Fractional`: floating-point types (`Float` and `Double`)
    - `(/) :: a -> a -> a`: fractal division
    - `recip :: a -> a`: fractional reciprocation (i.e. `1/x`)

# Defining functions

In Haskell, for `if` _conditionals_, both the `then` and `else` branches must return a value of the same type:

```haskell
isPositive :: (Num a, Ord a) => a -> Bool
isPositive x = if x > 0 then True else False
```

The same function can also be expressed using _guards_:

```haskell
isPositive' :: (Num a, Ord a) => a -> Bool
isPositive' x
  | x > 0 = True
  | otherwise = False
```

The `|` sumbol is read as "such that". Unlike the `else` clause, an `otherwise` clause (which is defined as `True`) is _not_ mandatory, this means a function is not required to be _total_, i.e. to handle all possible cases.

Functions can be defined using _pattern matching_, in which arguments are matched—fully or partially—against specific values:

```haskell
factorial :: Integral a => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)
```

Pattern matching also works for tuples and lists:

```haskell
middle :: (a, b, c) -> b
middle (_, x, _) = x

isPrefixA :: [Char] -> Bool
isPrefixA ('A':_) = True
isPrefixA (_:_) = False
```

_Lambda expressions_ are functions without a name. They are useful when passed as an argument to or returned from a function:

```haskell
applyTwice :: (Int -> Int) -> (Int -> Int)
applyTwice f = \x -> f (f x)

incrementBy :: Int -> (Int -> Int)
incrementBy x = \y -> x + y
```

```ghci
> succ 1
2
(applyTwice succ) 1

> (incrementBy 1) 3
4
> (incrementBy 5) 3
8
```

Lambda expressions using operators can be succinctly expressed using _operator sections_. Given an operator (e.g. `/` or `^`) and a first or second operand (`1` or `2`), an operation can be turned into a lambda function expecting the missing operand as follows:

```ghci
> map (1/) [1..5]
[1.0,0.5,0.3333333333333333,0.25,0.2]
> map (^2) [1..5]
[1,4,9,16,25]
```

# List comprehensions

In mathematics, comprehensions are used to create sets of existing sets, e.g. all numbers of the range from 1 to 5 squared: ${x^2 | x \in {1..5}}$. In Haskell, the same concept (albeit with a slightly different syntax) is used to create lists based on existing lists:

```ghci
> [x^2 | x <- [1..5]]
[1,4,9,16,25]
```

The symbol `|` is read as _such that_, and `<-` as _drawn from_, which the expression `x <- [1..5]` being called a _generator_. The values can be drawn from multiple generators:

```ghci
> [(n,c) | n <- [1, 2, 3], c <- ['a', 'b', 'c']]
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]
```

The values drawn from the left generator change less frequently than the ones drawn from the right one. (The left generator can be thought of as the outer and the right as the inner loop in terms of structured programming.)

Later generators can draw values from earlier generators:

```ghci
> [(x, y) | x <- [1, 2, 3], y <- [x..3]]
[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]
```

This mechanism is useful for applications such as concatenating lists:

```haskell
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]
```

The wildcard pattern `_` can be used to dismiss parts of drawn values:

```haskell
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _) <- ps]
```

The length of a list can be determined using the same technique:

```haskell
length :: [a] -> Int
length xs = sum [1 | _ <- xs]
```

Values drawn from generators can be filtered using _gaurds_:

```haskell
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
```

A table (i.e. a list of key-value pairs) can be filtered by a given key in order to find its respective value:

```haskell
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
```

The `zip` function pairs up values of two lists in tuples until the shorter of the two lists is exhausted:

```haskell
rank :: [a] -> [(Int, a)]
rank xs = zip [1..] xs
```

This technique can be used in combination with a list comprehension to check if a list is sorted in ascending order:

```haskell
ascending :: Ord a => [a] -> Bool
ascending xs = and [x <= y | (x, y) <- zip xs (tail xs)]
```

Or to find the indices of a value in a list:

```haskell
indices :: Eq a => a -> [a] -> [Int]
indices x xs = [i | (i, v) <- zip [0..] xs, v == x]
```

Since strings are lists of characters, they can be built using list comprehensions, which then are called _string comprehensions_:

```haskell
alphabet :: Int -> String
alphabet n = take n [c | c <- ['a'..]]
```
