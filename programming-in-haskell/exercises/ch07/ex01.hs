func :: (a -> Bool) -> (a -> b) -> [a] -> [b]
func p f xs = [f x | x <- xs, p x]

-- > func (\x -> x `mod` 2 == 0) (\x -> x * 2) [1..10]
-- [4,8,12,16,20]
func' :: (a -> Bool) -> (a -> b) -> [a] -> [b]
func' p f = map f . filter p
-- > func' (\x -> x `mod` 2 == 0) (\x -> x * 2) [1..10]
-- [4,8,12,16,20]
