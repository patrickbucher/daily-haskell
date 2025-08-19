-- > all' even [2,4,8,16,100]
-- True
-- > all' even [2,4,8,16,100,17]
-- False
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\acc -> \x -> acc && p x) True

-- > any' even [1,3,5,7,9]
-- False
-- > any' even [1,3,5,7,9,10]
-- True
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldl (\acc -> \x -> acc || p x) False

-- > takeWhile' even [2,4,6,7,8,10]
-- [2,4,6]
-- > takeWhile' even [1,2,4,6,7,8,10]
-- []
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

-- > dropWhile' even [2,4,6,8,9,10,11,12]
-- [9,10,11,12]
-- > dropWhile' even [1,2,4,6,8,9,10,11,12]
-- [1,2,4,6,8,9,10,11,12]
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
  | p x = dropWhile' p xs
  | otherwise = (x : xs)
