and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
  | x == True = and' xs
  | otherwise = False

concat' :: [[a]] -> [a]
concat' [] = []
concat' [[]] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x = x : replicate' (n - 1) x

nth' :: [a] -> Int -> a
nth' (x:_) 0 = x
nth' (_:xs) n = nth' xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (x':xs) = x == x' || elem' x xs
