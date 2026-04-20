# Getting Started with Types

Every value has a type. The most common types are:

- `Integer`: arbitrary large integer
- `Int`: 32-bit or 64-bit (platform-dependant) integer
- `Word`: unsigned integer value of the same size as `In`
- `Float`: single-precision floating-point number
- `Double`: double-precision floating-point number
- `Bool`: boolean values `True` and `False`
- `Char`: a single character (Unicode code point), e.g. `'A'`
- `String`: a list of characters, e.g. "abc"
- `[T]`: a list of type `T`
- `(X, Y)`: a tuple with two elements of type `X` and `Y`

The type declaration precedes the symbol's definition:

```haskell
number :: Int
number = 42

name :: String
name = "Joe"

one, two :: Int
one = 1
two = 2
```

In GHCi, surround such multi-line definitions using `:{` and `:}`:

```ghci
λ :{
  foo :: Float
  foo = 7.31
  }:
```

Or supply the type after the expression:

```ghci
λ foo = 7.31 :: Float
```

Get type information:

```ghci
λ :type "hello"
"hello" :: String
λ :t 'h'
'h' :: Char
```

Annotate the type of a function:

```haskell
hypot :: Float -> Float -> Float
hypot a b = sqrt $ a^2 + b^2
```

Since Haskell's functions are curried, above function applied to a single `Float` argument returns a function that takes another `Float` argument and returns a `Float` value.

TODO: p. 95 "Considering Readability …"
