find :: Eq a => a -> [a] -> [Int]
find x xs = [i | (i, x') <- zip [0 ..] xs, x' == x]
