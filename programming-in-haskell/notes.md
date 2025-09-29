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

# Recursive functions

A _recursive function_ is a function that is defined in terms of itself, e.g. this function to compute the factorial of a number:

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

The first equation is the _base case_, the second equation the _recursive case_. Recursion ultimately ends as the recursive case reduces the problem towards the base case.

Since Haskell has no loops, lists are processed element by element using recursion:

```haskell
sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + sum ns
```

Since 0 is the neutral element of the addition, the sum of the empty list is defined as 0 (base case). The sum of a non-empty list is the first element of the list added to the sum of its tail (recursive case).

The `zip` function accepts two lists and therefore requirs two base cases:

```haskell
zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys
```

In _multi recursion_, a function is defined in terms of multiple recursive calls, e.g. for calculating the Fibonacci numbers:

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)
```

In _mutual recursion_, two functions repeatedly call one another. A pair of functions that check if a number is odd or even, respectively, can be (inefficiently) implemented as follows:

```haskell
even :: Int -> Bool
even 0 = True
even n = odd (n - 1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n - 1)
```

A number is even when its predecessor is odd, or odd when its predecessor is even, respectively.

In general, recursive functions can be defined using this five-step process:

1. define the type
2. enumerate the cases
3. define the base case(s)
4. define the recursive case
5. generalise and simplify
    - types can be generalized (e.g. `Num` instead of `Int`)
    - definitions can be simplified (e.g. using library functions)
    - base cases can be merged

# Higher-order functions

A _higher-order function_ takes a function as an argument, returns a function as its results, or does both. Since functions returning other functions is denoted as currying in the appropriate context, the term higher-order function commonly refers to functions taking other functions as their arguments. Many higher-order functions for list processing are provided in the prelude.

The `map` function applies a given function to each element of a list, thereby producing a list of return values. This function can be defined using a list comprehension:

```haskell
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
```

For example:

```ghci
> map (*2) [1, 2, 3]
[2, 4, 6]
```

The `filter` function expects a predicate function to perform a test on each element of a list and returns a list of the elements satisfying the predicate:

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs, p x]
```

For example:

```ghci
> filter (\x -> x `mod` 2 == 0) [1,2,3,4,5]
[2, 4]
```

Some additional often-used higher-order functions from the prelude are:

- `all`: checks if all elements satisfy a predicate
- `any`: checks if at least one element satisfies a predicate
- `takeWhile`: selects element from a list as long as they satisfy a predicate
- `dropWhile`: discards element from a list as long as they satisfy a predicate

Many functions `f` processing lists using an operator `#` can be defined using this recursive pattern:

```haskell
f :: [a] -> a
f [] = v
f (x:xs) = x # f xs
```

The operator `#` is applied to the head of the list, and its tail is processed recursively until the base case matches the empty list, for which the pre-defined value `v` is returned.

The `foldr` (_fold right_) function encapsulates this behaviour and accepts the operator `#` as well as the value `v` as an argument:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v
foldr f v (x:xs) = f x (foldr f v xs)
```

This function folds up the list by applying the function to the list's head and to the result of folding the list's tail. The operator `f` is assumed to associate to the right, with the list's last element and the value `v` being processed first:

```plain
> foldr (+) 0 [1, 2, 3]
= 1 + (foldr (+) 0 [2, 3])
= 1 + (2 + (foldr (+) 0 [3]))
= 1 + (2 + (3 + (foldr (+) 0 [])))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
```

The function `foldl` implements the same mechanism for left-associative operators:

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
```

While `foldr` first recursively builds up a sequence of calculations, which are then performed, `foldl` processes the initial value using the operator and passes this _accumulator_ along to the recursive call:

```plain
> foldl (+) 0 [1, 2, 3]
= foldl (+) (0 + 1) [2, 3]
= foldl (+) 1 [2, 3]
= foldl (+) (1 + 2) [3]
= foldl (+) 3 [3]
= foldl (+) (3 + 3) []
= foldl (+) 6 []
= 6
```

The higher-order operator `.` composes two functoins, returning a new function:

```haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)
```

This is read as _f composed with g_ and can be applied as follows:

```ghci
> ((+1) . (*2)) 3
7
```

# Declaring types and classes

New types can be declared in terms of existing types using the `type` mechanism:

```haskell
type String = [Char]
type Position = (Int, Int)
type Move = Position -> Position
```

The name of the new type must start with a capital letter.

Types must not be recursive:

```haskell
type Tree (Int, [Tree])
```

For this purpose, the `data` mechanism (see below) can be used.

Type declarations can use parameters:

```haskell
type Pair a = (a, a)
type Table k v = [(k, v)]
```

New types can be declared using the `data` mechanism:

```haskell
data Bool = False | True
data Directon = Up | Right | Down | Left
```

The `|` is read as _or_ and separates the _constructors_, which must have globally unique names. The names have no particular meaning on their own but must be defined by the programmer.

A constructor can accept arguments:

```haskell
data Shape = Circle Float | Rect Float Float | Square Float
```

Those constructors are automatically turned into (curried) functions:

```haskell
Circle :: Float -> Shape
Rect :: Float -> Float -> Shape
Square :: Float -> Shape
```

Data declarations can be parametrised:

```haskell
data Maybe a = Nothing | Just a
```

This mechanism can be used to create safe versions of the `div` and `tail` functions:

```haskell
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safetail :: [a] -> Maybe [a]
safetail [] = Nothing
safetail xs = Just (tail xs)
```

For a new type with a single constructor and a single argument, the `newtype` mechanism can be used:

```haskell
newtype Nat = N Int
```

`Nat` and `Int` are different types—unlike the result of the declaration `type Nat = Int`, which introduces the type name `Nat` as a mere synonym for `Int`. Unlike the `data` mechanism, `newtype` does not incur a perrformance penalty.

Unlike `type`, `data` allows for recursive definitions:

```haskell
data Nat = Zero | Succ Nat
```

This type can be used to express numbers in a recusive manner. These two functions connect between those two worlds:

```haskell
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))
```

In order to display values of the `Nat` type, the `Show` type class must be derived (more of which further below):

```haskell
data Nat = Zero | Succ Nat
  deriving Show
```

A new list type can be defined in terms of `Cons` cells:

```haskell
data List a = Nil | Cons a (List a)

listLength :: List a -> Int
listLength Nil = 0
listLength (Cons _ x) = 1 + (listLength x)
```

Which can be used as follows:

```ghci
> listLength (Cons 3 (Cons 2 (Cons 1 Nil)))
3
```

A binary tree can be declared as follows:

```haskell
data Tree a = Leaf a | Node (Tree a) a Node (Tree a)
```

It either is a `Leaf` with a single value or a `Node` with a left subtree, a value, and a right subtree.

Type classes are declared using the `class` mechanism:

```haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x /= y = not (x == y)
```

(Type classes are similar to _abstract classes_ in object-oriented programming languages, which can pre-define some operations, and requir the implementation of other operations by a concrete class.)

A specific instance (i.e. a type) of that class must fill in the missing definitions (i.e. `==`, in whose terms `/=` is already provided):

```haskell
instance Eq Bool where
  False == False = True
  True == True = True
  _ == _ = False
```

A class can be defined as extending another class. E.g. the `Ord` class requires its types to be of class `Eq` and support additional operations:

```haskell
class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  min, max :: a -> a -> a

  min x y | x <= y = x
          | otherwise = y
  max x y | x <= y = y
          | otherwise = x
```

A type can be automatically made an instance of another type using the `deriving` mechanism:

```haskell
data Bool = False | True
            deriving (Eq, Ord, Show, Read)
```

The order of constructor definitions is used to establish the `Ord` implementation: because `False` appears before `True`, `False < True` holds true.

For parametrized types, `deriving` requires that the parameter type satisfies the deriving type.

# Interactive programming

In a pure language like Haskell, side effects are implemented by accepting the state of the entire world as an argument and returning an updated version of it, which is expressed by the `IO` type:

```haskell
type IO = World -> World
```

In order to allow for effectful functions returning a value, this type is parametrized:

```haskell
type IO a = World -> (a, World)
```

A function returning the type `IO Char` produces a character using a side-effect, while `IO ()` is an effectful functions that returns nothing.

In fact, since it's infeasible to pass the entire state of the world around as a function argument, the `IO` type is implemented as a built-in language primitive. However, the above explanation serves as a useful mental model to understand effectful functions in the context of a pure language.

Characters can be read and written using the following functions:

```haskell
getChar :: IO Char
putChar :: Char -> IO ()
```

The `return` function is a bridge from the pure to the impure world. There's no way back—once impure, always impure:

```haskell
return :: a -> IO a
```

The `do` notation combines a sequence of `IO` actions into a single composite action:

```haskell
do v1 <- a1
   v2 <- a2
   ...
   return (f v1 v2 ...)
```

The result of action `a1` is stored in `v1`, the result of action `a2` in `v2`, etc. (The `<-` syntax is the same that is used for generators in list comprehensions.)

The `do` block allows for transparent handling of effectful functions, but requires re-wrapping of the stripped `IO` type using `return`. Within the `do` block, the type `IO a` is automatically unwrapped as `a`. However, the `IO` has to be put back into the type using return. If `f` produces a value of type `b`, then `return` produces a value of type `IO b`.

The provided `getChar` and `putChar` functions can be combined to handle the input and output of entire lines of text:

```haskell
getLine :: IO String
getLine = do
  c <- getChar
  if c == '\n'
    then return []
    else do
      cs <- getLine
      return
      (c : cs)

putString :: String -> IO ()
putString [] = return ()

putString (c:cs) = do
  putChar c
  putString cs

putLine :: String -> IO ()
putLine cs = do
  putString cs
  putChar '\n'
```

# Unbeatable tic-tac-toe

The game _tic-tac-toe_ is played on a 3x3 grid by two players, X and O, which take turns putting their mark in the grid. The player to place three of his marks in a row—horizontally, vetically, or diagonally—is the winner. If the grid is filled without any player creating a winning row, the game ends in a draw. When playing perfectly, each player can force a draw, no matter in which order they begin.

Implementing the game requirs functinoality from these modules:

```haskell
import Data.Char
import Data.List
import System.IO
```

The grid size shall be adjustable:

```haskell
size :: Int
size = 3
```

The grid is a list of rows of possible player marks:

```haskell
type Grid = [[Player]]
```

Whereas the players—X, O, and B for "blank"—are a new data type:

```haskell
data Player
  = O
  | B
  | X
  deriving (Eq, Ord, Show)
```

By deriving the `Ord` trait, the order is given as `O < B < X`.

Players are swapped as follows, with the case for `B` being only included for completeness:

```haskell
next :: Player -> Player
next O = X
next B = B
next X = O
```

The game starts on an empty grid:

```haskell
empty :: Grid
empty = replicate size (replicate size B)
```

The grid is full if there are no blanks left:

```haskell
full :: Grid -> Bool
full = all (/= B) . concat
```

The next turn's player is determined by th enumber of present marks:

```haskell
turn :: Grid -> Player
turn g =
  if os <= xs
    then O
    else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g
```

Player O is assumed to go first because `turn empty` evaluates to `O`.

If a player is the winner on a given grid is determined as follows:

```haskell
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]
```

The `transpose` function from `Data.List` flips a grid represented as a list of rows into a list of columns.

The main diagonal of a grid is determined by collecting the fields with equal row and column index:

```haskell
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]
```

The grid is printed line by line (note: this is a simplified implementation compared to the book):

```haskell
putGrid :: Grid -> IO ()
putGrid = putStrLn . concat . interleave "\n" . map showRow
```

The `interleave` function puts a separator in between a sequence of items:

```haskell
interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys
```

This can also be used as a column separator to output rows:

```haskell
showRow :: [Player] -> String
showRow = foldr1 (++) . interleave "|" . map showPlayer
```

The player mark is printed as a space for blanks and using the derived `show` implementation for player marks:

```haskell
showPlayer :: Player -> String
showPlayer B = " "
showPlayer x = show x
```

A grid can now be printed as follows:

```ghci
> putGrid [[B,O,O],[O,X,O],[X,X,X]]
 |O|O
O|X|O
X|X|X
```

In order to identify moes, the flattened grid is indexed by the natural numbers from 0 to 8. This range, and the initial emptiness of the picked field, needs to be validated:

```haskell
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B
```

A valid move is applied to the grid, and a new version of the grid is returned, wrapped in a `Maybe`: for an invalid move, `Nothing` is returned; for a valid move, `Just` the updated grid:

```haskell
move :: Grid -> Int -> Player -> Maybe Grid
move g i p =
  if valid g i
    then Just (chop size (xs ++ [p] ++ ys))
    else Nothing
  where
    (xs, B:ys) = splitAt i (concat g)
```

The flattened grid is split at the given index `i`, which must match a blank field `B`.

The `chop` function turns the flattened and updated grid back into its previous row-column structure:

```haskell
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
```

## Human Player Interaction

The player shall be prompted to enter his move:

```haskell
prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
```

Since the player must only enter a natural number as its move, a function to only read such input shall be defined, using the `isDigit` predicate function from `Data.Char`:

```haskell
getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt
```

The function `tictactoe` starts the game:

```haskell
tictactoe :: IO ()
tictactoe = run empty O
```

The game starts on an empty grid with player O. Two mutually recursive functions—`run` (for drawing and top-level control) and `run'` (to handle individual moves)—are requird for playing the game:

```haskell
run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p
```

The function `putGrid` was defined earlier.

The function `cls` clears the screen:

```haskell
cls :: IO ()
cls = putStr "\ESC[2J"
```

The function `goto` continues drawing at a particular x and y coordinate on the screen:

```haskell
goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
```

The handling of a move is implemented in `run'`, which returns control to the `run` function after a move has been performed:

```haskell
run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      Nothing -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      Just g' -> run g' (next p)
```

## Implementing a Minimax Bot

A game tree representing all possible moves and their outcomes starting from the empty grid can be represented using this generic tree structure:

```haskell
data Tree a =
  Node a [Tree a]
  deriving (Show)
```

Each `Node` consists of a list of `Tree` items. A leaf is a `Node` without any children, i.e. with an empty list of `Tree` items. The type parameter `a` allows for any type being represented as such a tree.

The entire game tree can be built up recursively by processing all possible moves from a given position:

```haskell
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]
```

The possible moves from a given grid are determined using the `moves` function:

```haskell
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [maybeToList (move g i p) | i <- [0 .. ((size ^ 2) - 1)]]
  where
    maybeToList (Just x) = [x]
    maybeToList Nothing = []

won :: Grid -> Bool
won g = wins O g || wins X g
```

A tree's depth, and the time requird to build it up, can be limited by _pruning_ it:

```haskell
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]
```

Thanks to lazy evaluation, `prune 5 (gametree empty O)` won't generate more than five levels in the first place. However, with a simple game like tic-tac-toe, modern hardware allows for building up the entire game tree in little time:

```haskell
depth :: Int
depth = 9
```

Given a game tree, the best next move can be determined using the _minimax_ algorithm.

First, every node needs to be labelled with a player:

- Leaves are labelled with the winning player `O` or `X`, or `B` in case of a draw.
- Other nodes are labelled depending on whose player's turn it is on the basis of the labels of the child nodes:
    - On player O's turn, the _minimum_ of the children's labels.
    - On player X's turn, the _maximum_ of the children's labels.

The choice of minimum and maximum is due to the relationship `O < B < X`: Agent O strives for the minimum, agent X for the maximum.

The tree of grids is labelled as follows, returning a tree of _labelled_ grids:

```haskell
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']
```

Second, the best move for a given player and grid is determined by picking the first child of the current game node in the three that matches the player with its label. (This is the same label as the one in the root node, since the tree is built up for every move from the current player's perspective.)

```haskell
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree
```

Notice that there may be multiple possible best moves leading to the best possible outcome, from which the first one is selected.

Since the minimax algorithm is computationally expensive, the script shall be turned into a compiled program with a `main` function:

```haskell
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty 0
```

Output buffering is disabled so that output appears on the screen at once.

The mutually recursive `run` and `run'` functions are replaced with the mutually recursive `play` and `play'` functions:

```haskell
play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      Nothing -> do
        putStrLn "ERROR: Invalid move"
        play' g p
      Just g' -> play g' (next p)
  | p == X = do
    putStr "Player X is thinking..."
    (play $! (bestmove g p)) (next p)
```

The operator `$!` forces evaluation of the best move to prevent a delay while playing.

The program `tic-tac-toe-bot.hs` can be compiled an run as follows:

```bash
ghc -dynamic -O2 tic-tac-toe-bot.hs
./tic-tac-toe-bot
```

# Monads and more

The level of generality of Haskell code can be increased by considering functions that are generic over a range of types such as lists, trees, and input/output actions.

## Functors

Consider these two functions that process the elements of a list in a similar manner but using a different transformation on each element:

```haskell
inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n ^ 2 : sqr ns
```

Both functions process a list of integer values one by one, but apply a different function to each element: `(+1)` and `(^2)`, respectively. Abstracting this difference out by passing that particular function as an additional parameter gives the already known `map` function:

```haskell
map :: (a - B b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs
```

Using `map`, the `inc` and `sqr` functions from above can be re-defined as follows:

```haskell
inc = map (+1)
sqr = map (^2)
```

The idea of applying a function to each element of a container is captured in a type class called `Functor` taht supports the mapping operation `fmap`:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

The type `f` is a `Functor` if it supports an operation `fmap` that takes a function of type `a -> b`, a container with elements of type `a`, and returns a container with elements of type `b`, i.e. the results of the transformation applied to all the elements.

Lists and the `Maybe` type are functors:

```haskell
instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap g (Just x) = Just (g x)
```

Which can be applied as follows:

```ghci
> fmap (+1) []
[]

> fmap (+1) [1..5]
[2,3,4,5,6]

> fmap (+1) Nothing
Nothing

> fmap (+1) (Just 1)
Just 2
```

Consider this user-defined binary tree type of node and leaves:

```haskell
data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)
```

It can be turned into a functor by implementing the `fmap` function for it:

```haskell
instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)
```

Which then can be used as follows:

```ghci
> fmap (+1) (Leaf 1)
Leaf 2

> fmap (+1) (Node (Leaf 1) (Leaf 2))
Node (Leaf 2) (Leaf 3)
```

### Functor laws

The function `fmap` has to adhere to the following two _functor laws_:

1. `fmap id = id` states that `fmap` preserves the identity function including its type.
2. `fmap (g . h) = fmap g . fmap h` states that `fmap` preserves function composition including its type, but also the order and amount of elements.

## Applicatives

Functors apply single-argument functions to their wrapped values. _Applicatives_ generalise this idea by supporting multi-argument functions. Instead of defining a series of functors, one for each argument count, which would be impractical, applicatives make use of currying to support a variable number of arguments.

This can be achieved using the two following definitions:

```haskell
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
```

The `pure` function wraps a value of type `a` in a structure of type `f`. The `<*>` operator is a generalised form of functoin application: The argument value, the argument function, and the return value are wrapped in `f` structures. If a function `g` is applied to the arguments `x`, `y`, and `z`:

```haskell
g x y z
```

Its _applicative style_ invocation looks as follows:

```haskell
pure g <*> x <*> y <*> z
```

However, in applicative style, the arguments are all of type `f a` instead of `a`.

A series of `fmap` functions supporting different numbers of arguments can be defined as follows:

```haskell
fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y

fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g x y z = pure g <*> x <*> y <*> z
```

The `Applicative` type class, defining _applicative functors_, is pre-defined as follows:

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

The `Maybe` functor is already defined as an applicative:

```haskell
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just g) <*> mx = fmap g mx
```

This allows for _exceptional_ programming by chaining possibly missing values without manually propagating errors:

```ghci
> pure (+1) <*> Nothing
Nothing
> pure (+1) <*> Just 5
Just 6
> pure (+) <*> Just 2 <*> Nothing
Nothing
> pure (+) <*> Just 2 <*> Just 3
Just 5
```

The list is also already defined as an applicative:

```haskell
instance Applicative [] where
  pure x = [x]
  gs <*> xs = [g x | g <- gs, x <- xs]
```

An empty list represents failure, whereas a non-empty list contains all the possible results from a computation:

```ghci
> pure (+1) <*> [1,2,3]
[2,3,4]

> pure (+) <*> [1] <*> [2]
[3]

> pure (*) <*> [1,2] <*> [3,4]
[3,4,6,8]
```

In the last of the above examples, exery element of the first list is multiplied with every element of the second list. The resulting list contains the results of all the possible ways the integers in the two given lists can be multiplied.

The `IO` type is defined as an applicative, too:

```haskell
instance Applicative IO where
  pure = return
  mg <*> mx = do
    g <- mg
    x <- mx
    return (g x)
```

This allows for the definition of functions such as the following, which reads a number of characters from the keyboard:

```haskell
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)
```

Using this applicative style for `IO` allows for applying pure functions to impure arguments without the need to deal with the sequencing operations manually.

### Effectful programming

In applicative style, functions are applied to arguments that may have effects: the possibility of failure, many ways to succeed, or performing input/output actions. Applicatives abstract the idea of applying pure functions to effectful arguments, where the nature of the effect is determined by their underlying functors. This allows for the definition of generic functions that can be used by any applicative function, such as:

```haskell
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs
```

This pre-defined function transforms a list of applicative actions into a single such action that returns a list of values. Using `sequenceA`, the `getChars` function from before can be re-defined as follows:

```haskell
getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar)
```

### Applicative laws

Applicative functors are required to satisfy  the following _applicative laws_:

1. `pure id <*> x = x`: `pure` preserves the identity function
2. `pure (g x) = pure g <*> pure x`: `pure` preserves function application
3. `x <*> pure y = pure (\g -> g y) <*> x`: the evaluation order of the two components doesn't matter
4. `x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z`: the `<*>` operator is associative

Haskell provides an infix operator for `fmap`, defined by `g <$> x = fmap g x`. So instead of using `pure` explicitly:

```haskell
pure g <*> x <*> y <*> z
```

The shorter notation can be used:

```haskell
g <$> x <*> y <*> z
```

## Monads

Consider this data type to express division for natural numbers:

```haskell
data Expr
  = Val Int
  | Div Expr Expr
```

Such expressions can be evaluated as follows:

```haskell
eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y
```

This definition allows for unsafe operations:

```ghci
> eval (Div (Val 1) (Val 0))
*** Exception: divide by zero
```

A safe version of division takes this into account by using the `Maybe` type:

```haskell
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)
```

However, using this safe division requirs the evaluation to make the according distinctions:

```haskell
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) =
  case eval x of
    Nothing -> Nothing
    Just n ->
      case eval y of
        Nothing -> Nothing
        Just m -> safediv n m
```

This new definition solves the issue from before:

```ghci
> eval (Div (Val 1) (Val 0))
Nothing
```

However, the two nested `case` expressions render this implementation rather verbose.

A definition using applicative style would be more concise:

```haskell
eval :: Expr -> Maybe Int
eval (Val n) = pure n
eval (Div x y) = pure safediv <*> eval x <*> eval y
```

Unfortunately, the types of `safediv` (`Int -> Int -> Maybe Int`) deviates from the one required (`Int -> Int -> Int`). Applicatives are only the solution if pure functions shall be applied to effectful arguments, but not if the function is effectful, as in the case of `safediv`, which can fail to produce a result.

The common pattern of `eval`—mapping `Nothing` to itself, and `Just x` to a computation involving `x`—can be abstracted out using the `>>=` operator:

```haskell
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f =
  case mx of
    Nothing -> Nothing
    Just x -> f x
```

The `>>=` operator takes an argument of type `a` that may fail, a function of type `a -> b` that may fail, and returns a result of type `b` that may fail. The failure of a missing argument is propagated, whereas a present argument value is processed using the given function `f`. Thus, the `>>=` operator integrates the sequencing of values of the `Maybe` type with the processing of their results. The `>>=` operator is pronounced as _bind_, because its second argument binds the first.

The `eval` definition can be simplified using the `>>=` operator:

```haskell
eval :: Expr -> Maybe Int
eval (Val n) = pure n
eval (Div x y) = eval x >>= \n -> eval y >>= \m -> safediv n m
```

In general, a typical expression built using the `>>=` operator has the following structure:

```haskell
m1 >>= \x1 ->
m2 >>= \x2 ->
…
mn >>= \xn ->
f x1 x2 … xn
```

The expressions `m1`, `m2`, …, `mn` are evaluated in turn and their resulting values `x1`, `x2`, …, `xn` combined by applying the function `f`. The expression as a whole succeeds if every component (`m1`, `m2`, …, `mn`) succeeds.

The `do` notation simplifies expressions such as the one above:

```haskell
do
  x1 <- m1
  x2 <- m2
  …
  xn <- mn
  f x1 x2 … xn
```

Using this `do` notation, `eval` can be further simplified:

```haskell
eval :: Expr -> Maybe Int
eval (Val n) = pure n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safediv n m
```

The `do` notation can be used with any monad, which is pre-declared as follows:

```haskell
class Applicative m =>
      Monad m
  where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  return = pure
```

A monad is an applicative that supports `return` and `>>=`, which `return` just being an alias for `pure`.

### The State Monad

Consider a type for functions tat manipulate state, e.g. a natural number:

```haskell
type State = Int
```

Such a function is a state transformer, which also returns a value alongside the updated state:

```haskell
type ST a = State (a, State)
```

Such a function can also take an additional argument using currying. A state transformer accepting a label and returning a natural number can be declared as `Char -> ST Int`, which is an abbreviation for `Char -> State -> (Int, State)`.

Unfortunately, types declared using the `type` mechanism cannot be made into instances of classes. Therefore, `ST` is redefined as follows (using a dummy constructor `S`):

```haskell
newtype ST a =
  S (State -> (a, State))
```

A utility application function removes this dummy constructor:

```haskell
app :: ST a -> State -> (a, State)
app (S st) x = st x
```

`ST` can be turned into a functor as follows:

```haskell
instance Functor ST where
  fmap g st =
    S
      (\s ->
         let (x, s') = app st s
          in (g x, s'))
```

(The `let` keyword allows for variable definitions that then can be used within the `in` expression.) This functor allows for function application to the result value of a state transformer.

Next, `ST` is turned into an applicative as follows:

```haskell
instance Applicative ST where
  pure x = S (\s -> (x, s))
  stf <*> stx =
    S
      (\s ->
         let (f, s') = app stf s
             (x, s'') = app stx s'
          in (f x, s''))
```

The `pure` function transforms a value into a state transformer that returns this value without modifying the state.

The `<*>` operator:

1. applies a state transformer
2. that returns a function to a state transformer
3. that returns an argument to give a state transformer
4. that returns the result of applying the function to an argument

Finally, `ST` is turned into a monad as follows:

```haskell
instance Monad ST where
  st >>= f =
    S
      (\s ->
         let (x, s') = app st s
          in app (f x) s')
```

The `>>=` operator:

1. applies the state transformer `st` to the initial state `s`
2. then applies the function `f` to the value `x`
3. which gives a new state transformer `f x`
4. which then is applied to the new state `s'` to give the final result

TODO: relabeling trees (p. 171)
