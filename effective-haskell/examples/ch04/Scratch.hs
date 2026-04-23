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
