# Getting Started with Haskell

Start the Haskell REPL GHCi:

```sh
$ ghci
Prelude>
λ
```

Modify `$HOME/.ghci` to set an alternative prompt:

```plain
:set prompt "λ "
```

Use the unicode code point `U+03bb` to get the `λ` character.

Use arithmetic operators:

```ghci
λ 1 + 2
3
λ 3 + 4 - 5
2
λ 4 * (4 - 1) / 3
4.0
```

## Lists

Define some lists:

```ghci
λ [1,2,3]
[1,2,3]
λ ["one","two","three"]
["one","two","three"]
```

Define numeric lists using the range syntax:

```ghci
λ [1..10]
[1,2,3,4,5,6,7,8,9,10]
λ [5..1]
[]
```

Define the increment by example:

```ghci
λ [1,3..10]
[1,3,5,7,9]
λ [9,7..0]
[9,7,5,3,1]
```

Enclose multi-line definitions within `:{` and `:}`:

```ghci
λ :{
  [1
  ,2
  ,3
  ,4
  ]
  :}
[1,2,3,4]
```

## Tuples

Create tuples (consisting of different types):

```ghci
λ double = (1, "two")
λ triple = (1, "two" 3.0)
λ quadruple = (1, "two", 3.0, False)
```

Access the first and second element of a two-element tuple:

```ghci
λ fst ("one", 2)
λ snd ("one", 2)
```

Tuples of lists, lists of tuples:

```ghci
λ ([1,2,3],[4,5],[6,7,8,9])
([1,2,3],[4,5],[6,7,8,9])
λ [(1,2),(3,4),(4,5)]
[(1,2),(3,4),(4,5)]
```

Lists must contain tuples of the same type and size:

```ghci
λ [("one",1),("two",2),("three",3)]
[("one",1),("two",2),("three",3)]
λ [("one",1), (2,"two")]
ERROR
λ [(1,2),(3,4),(5,6,7)]
ERROR
```

## Strings and Output

Print strings, with and without a trailing newline:

```ghci
λ putStrLn "Hello, World!"
Hello, World!
λ putStr "Hello, World!"
Hello, World!λ
```

Use the `show` function to convert other types to strings:

```ghci
λ putStrLn (show 13)
13
λ putStrLn (show [1..3])
[1,2,3]
λ putStrLn (show (True,False))
(True,False)
```

Use `print`, which combines `putStrLn` with `show`:

```ghci
λ print [1..3]
[1,2,3]
```

Write a self-containing "Hello, World!" program (`Hello.hs`):

```haskell
module Main where
main = print "Hello, World!"
```

Compile the program using GHC and run it:

```sh
$ ghc Hello # or ghc Hello.hs
$ ./Hello
"Hello, World!"
```

Use the `runhaskell` utility for single runs:

```sh
$ runhaskell Hello.hs
```

Run the module into GHCi:

```ghci
λ :load Hello
λ main
"Hello, World!"
```

## Variables

Shadow existing variables:

```ghci
λ one = 1
λ two = one + one
λ one = 42
λ one
42
λ two
2
```

Only re-define variable names in GHCi, not in proper Haskell programs.

Even though the underlying value of `one` changed, `two` is still based on its old value.

Shadow a variable recursively:

```ghci
λ two = 1
λ two = two + 1
λ two
^CInterrupted.
```

Since Haskell is a lazy language, it lets you define infinite computations (`two = two + 1`), which will only fail upon evaluation.

By convention, variables suffixed with `'` are used to indicate its evolution:

```ghci
λ x = 1
λ x' = x + 1
λ x'' = x + x'
λ (x,x',x'')
(1,2,3)
```

## Functions

Join strings using the list concatenation operator `<>`:

```ghci
λ salutation = "Hello"
λ person = "George"
λ greeting = salutation <> " " <> person
λ greeting
"Hello George"
```

Define and call a _named_ function:

```ghci
λ greet salutation person = salutation <> " " <> person
λ greet "Hello" "Joe"
"Hello Joe"
```

Define and call a _anonymous_ or _lambda_ function:

```ghci
λ (\salutation person -> salutation <> " " <> person) "Hello" "Joe"
"Hello Joe"
```

Partially apply a function (_η-reduction_):

```ghci
λ add a b = a + b
λ addTwo = add 2
λ addTwo 3
5
```

Partially apply infix operators:

```ghci
λ twice = (*2)
λ halve = (/2)
λ recip = (1/)
λ twice 3
6
λ halve 12
6.0
λ recip 2
0.5
```

Use regular function as an infix operator:

```ghci
λ "Hello" `greet` "John"
"Hello John"
```

Use the right-associative _function application operator_ `$`, which has a very low precedence, instead of nested parentheses:

```ghci
λ twice (recip (halve 10))
0.4
λ twice $ recip $ halve 10
0.4
```

Use the _function composition operator_ `.` to combine two functions into one:

```ghci
λ f x = x + 1
λ g x = x * 2
λ f(g(3))
7
λ (f . g) 3
7
λ f . g $ 3
7
```

Use _pointfree_ style to reduce explicit parameters:

```ghci
λ twice x = (*) 2 x
λ twice = (*) 2
```

## Operators

Define a new operator to calculate the hypotenuse of two catheti in _prefix_ style:

```ghci
λ (@) a b = sqrt $ a^2 + b^2
λ 3 @ 4
5.0
```

Define the same operator in _infix_ style:

```ghci
λ a @ b = sqrt $ a^2 + b^2
```

Figure out associativity and precedence of an operator:

```ghci
λ :info @
infixl 9 @
```

The operator is left-associative (`infixl`) and has the highest precedence (9). The higher the precedence, the stronger the operator binds, e.g. `*` has precedence 7, `+` has precedence 6.

Define the operator with a lower precedence:

```ghci
λ a @ b = sqrt $ a^2 + b^2; infixl 5 @
```

In GHCi, the declaration has to take place on a single line. In a source code file, it can stand on a separate line.

## Bindings

Use `let`/`in` to define bindings to be used in an expression:

```haskell
greet salutation firstName lastName =
  salutation <> ", " <> firstName <> " " <> lastName

greet' salutation firstName lastName =
  let fullName = firstName <> " " <> lastName
  in salutation <> ", " <> fullName
```

Earlier `let` bindings can be used in subsequent ones. In fact, the order does not matter, so that forward references are also allowed.

Bindings defined in a `where` block can be used inside the function body:

```haskell
greet' salutation firstName lastName =
  salutation <> ", " <> fullName
  where fullName = firstName <> " " <> lastName
```

No `let` bindings will be available within `where`.

Use `let` bindings for intermediary values and `where` bindings for auxiliary functions.

## Conditionals

Use an `if`/`then`/`else` expression:

```haskell
formatMoney currency amount =
  if currency == "USD"
  then show amount <> " $"
  else currency <> " " <> (show amount)
```

Next `if`/`then`/`else` expressions:

```haskell
rate stars =
  if stars == 5
  then "great"
  else
    if stars == 4
    then "good"
    else
      if stars == 3
      then "mediocre"
      else
        if stars == 2
        then "insufficient"
        else
          if stars == 1
          then "horrible"
          else "unknown"
```

Use _gard clauses_ to simplify such expressions:

```haskell
rate' stars
  | stars == 5 = "great"
  | stars == 4 = "good"
  | stars == 3 = "mediocre"
  | stars == 2 = "insufficient"
  | stars == 1 = "horrible"
  | otherwise = "unknown"
```

Implement _FizzBuzz_:

```haskell
fizzbuzz n =
  if n == 0
  then ""
  else fizzbuzz (n - 1) <> "\n" <> denote n
  where
    denote n
      | n `rem` 15 == 0 = "FizzBuzz"
      | n `rem` 5 == 0 = "Buzz"
      | n `rem` 3 == 0 = "Fizz"
      | otherwise = show n
```
