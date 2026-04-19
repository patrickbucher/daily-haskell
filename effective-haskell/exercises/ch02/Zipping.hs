zipWith' :: (a -> b -> (a,b)) -> [a] -> [b] -> [(a,b)]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
