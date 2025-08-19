-- dec2int [2,3,4,5]
-- 2345
dec2int :: [Int] -> Int
dec2int = foldl (\acc -> \x -> acc * 10 + x) 0
