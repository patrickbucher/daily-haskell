# Creating New Types

Create a new type with a type constructor:

```haskell
data Coin = Coin String Float
```

Construct values from that type:

```ghci
λ nickel = Coin "Nickel" 0.05
λ dime = Coin "Dime" 0.10
```

These types are called _product types_, because with every additional field, the number of possible values multiplies.

Use pattern matching to access the fields:

```haskell
formatCoin :: Coin -> String
formatCoin (Coin name value) = name <> ": " <> show value
```

Write accessor functions for individual fields:

```haskell
coinName :: Coin -> String
coinName (Coin name _) = name

coinValue :: Coin -> Float
coinValue (Coin _ value) = value
```

Use _record syntax_ with named fields:

```haskell
data Coin = Coin
  { name :: String
  , value :: Float
  }
```

Use named arguments to initialize a record, which allows for an arbitrary field order:

```ghci
λ quarter = Coin { name = "quarter", value = 0.25 }
```

Accessor functions are automatically generated:

```ghci
λ name quarter
"quarter"
λ value quarter
0.25
```

Use _record update syntax_ to get an updated value:

```ghci
λ quarter Coin { name = "quarter", value = 0.25 }
λ inflated = quarter { value = 0.24 }
λ value quarter
0.25
λ value inflated
0.24
```

TODO: p. 123
