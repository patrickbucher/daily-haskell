isPositive :: (Num a, Ord a) => a -> Bool
isPositive x =
  if x > 0
    then True
    else False

isPositive' :: (Num a, Ord a) => a -> Bool
isPositive' x
  | x > 0 = True
  | otherwise = False
