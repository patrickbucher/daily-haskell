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
qsort (x:xs) =
    qsort smaller ++ [x] ++ qsort larger
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
seqn (act:acts) =
    do
        x <- act
        xs <- seqn acts
        return (x:xs)
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


