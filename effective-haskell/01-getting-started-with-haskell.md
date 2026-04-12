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
