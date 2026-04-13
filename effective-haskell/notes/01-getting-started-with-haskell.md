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

Enclose multi-line definitions within `:{` and `}:`:

```ghci
λ :{
  [1
  ,2
  ,3
  ,4
  ]
  }:
[1,2,3,4]
```

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

Join strings using the list concatenation operator `<>`:

```ghci
λ salutation = "Hello"
λ person = "George"
λ greeting = salutation <> " " <> person
λ greeting
"Hello George"
```
