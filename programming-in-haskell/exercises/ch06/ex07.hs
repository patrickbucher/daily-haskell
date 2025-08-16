merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [x] [] = [x]
merge [] [y] = [y]
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys
