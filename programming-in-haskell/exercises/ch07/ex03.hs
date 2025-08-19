-- > map' (*2) [1,2,3]
-- [2,4,6]
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> \acc -> (f x):acc) []

-- > filter' even [0..5]
-- [0,2,4]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x -> \acc -> if p x then x:acc else acc) []
