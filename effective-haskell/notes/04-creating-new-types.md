# Creating New Types

## Product Types

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

Enable the `RecordWildCards` language extension using a _language pragma_ in a source code file:

```haskell
{-# LANGUAGE RecordWildCards #-}
```

Enable the `RecordWildCards` language extension in GHCi:

```ghci
λ :set -XRecordWildCards
λ :seti -XRecordWildCards
```

`:seti` only enables the extension for interactively entered code, whereas `:set` also affects external code loaded into the session.

The `RecordWildCards` extension allows to binds all record fields to variables using the matching pattern `{..}`:

```haskell
describeCoin :: Coin -> String
describeCoin Coin{..} =
  name <> " USD " <> show value

createCoin :: String -> Float -> Coin
createCoin s f =
  let
    name = s
    value = f
  in
    Coin{..}
```

Record field names must be unique:

```haskell
data Book = Book
  { title :: String
  , author :: String
  , pages :: Int
  }

data Song = Song
  { artist :: String
  , title :: String -- ERROR: duplicate declaration
  , duration :: Float
  }
```

Either put the record declarations into different _modules_ or use prefixes:

```haskell
data Book = Book
  { bookTitle :: String
  , bookAuthor :: String
  , bookPages :: Int
  }

data Song = Song
  { songArtist :: String
  , songTitle :: String
  , songDuration :: Float
  }
```

## Sum Types

Create a type using a fixed set of values:

```haskell
data Direction = North | East | South | West
```

Add data fields:

```haskell
data Operation
  = Add Int Int
  | Sub Int Int
  | Increment Int
  | Decrement Int

evaluate :: Operation -> Int
evaluate op =
  case op of
    Add a b -> a + b
    Sub a b -> a - b
    Increment a -> a + 1
    Decrement a -> a - 1
```

Match against, say, `Decrement{}` to ignore the fields.

Use those variants:

```ghci
λ evaluate $ Add 3 4
7
λ evaluate $ Sub 9 2
7
λ evaluate $ Increment 4
5
λ evaluate $ Decrement 9
8
```

Use record syntax for sum types:

```haskell
data Contact
  = Email { username :: String, domain :: String }
  | Phone { prefix :: String, number :: String }

formatContact :: Contact -> String
formatContact contact =
  case contact of
    Email { username, domain } -> username <> "@" <> domain
    Phone { prefix, number } -> prefix <> number

email = Email { username = "webmaster", domain = "paedubucher.ch" }
phone = Phone { prefix = "0041", number = "123456789" }
```

The variants (e.g. `Email` and `Phone`) can also be defined seperately and then be put into the data type (e.g. `Contact`) as fields:

```haskell
data EmailInfo = EmailInfo { username :: String, domain :: String }
data PhoneInfo = PhoneInfo { prefix :: String, number :: String }

data Contact
  = Email EmailInfo
  | Phone PhoneInfo 

formatContact :: Contact -> String
formatContact contact =
  case contact of
    Email EmailInfo { username, domain } -> username <> "@" <> domain
    Phone PhoneInfo { prefix, number } -> prefix <> number
```

Use a sum type to avoid partial functions:

```haskell
safeDivide :: Int -> Int -> DivisionResult
safeDivide _ 0 = Undefined
safeDivide x y = Some $ x `div` y
```

Use a type parameter for a type:

```haskell
data Result a = Empty | Value a
```

Express _peano numbers_ using a data type and according conversion functions:

```haskell
data Peano = Z | S Peano

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S x) = 1 + (fromPeano x)

toPeano :: Int -> Peano
toPeano 0 = Z
toPeano x = S (toPeano (x - 1))

eqPeano :: Peano -> Peano -> Bool
eqPeano Z Z = True
eqPeano Z _ = False
eqPeano _ Z = False
eqPeano (S x) (S y) = eqPeano x y

addPeano :: Peano -> Peano -> Peano
addPeano Z (S y) = (S y)
addPeano (S x) (S y) = S (addPeano x (S y))
```

Implement lists:

```haskell
data List a = Void | Cons a (List a)

fromList :: List a -> [a]
fromList Void = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Void
toList (x:xs) = Cons x (toList xs)
```

TODO: p. 144 "Functions as Values"
