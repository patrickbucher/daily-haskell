isPositive :: (Num a, Ord a) => a -> Bool
isPositive x =
  if x > 0
    then True
    else False

isPositive' :: (Num a, Ord a) => a -> Bool
isPositive' x
  | x > 0 = True
  | otherwise = False

factorial :: Integral a => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

middle :: (a, b, c) -> b
middle (_, x, _) = x

isPrefixA :: [Char] -> Bool
isPrefixA ('A':_) = True
isPrefixA (_:_) = False

applyTwice :: (Int -> Int) -> (Int -> Int)
applyTwice f = \x -> f (f x)

incrementBy :: Int -> (Int -> Int)
incrementBy x = \y -> x + y
