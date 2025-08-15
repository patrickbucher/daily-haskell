nested :: [(Int, Int)]
nested = concat [[(a, b) | b <- [3, 4]] | a <- [1, 2]]
