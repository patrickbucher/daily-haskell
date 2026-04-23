{-# LANGUAGE RecordWildCards #-}
data Coin = Coin
  { name :: String
  , value :: Float
  }

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

data Direction = North | East | South | West

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

data DivisionResult = Some Int | Undefined

safeDivide :: Int -> Int -> DivisionResult
safeDivide _ 0 = Undefined
safeDivide x y = Some $ x `div` y

data Result a = Empty | Value a

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

data List a = Void | Cons a (List a)

fromList :: List a -> [a]
fromList Void = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Void
toList (x:xs) = Cons x (toList xs)
