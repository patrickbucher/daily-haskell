concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length :: [a] -> Int
length xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

rank :: [a] -> [(Int, a)]
rank xs = zip [1 ..] xs

ascending :: Ord a => [a] -> Bool
ascending xs = and [x <= y | (x, y) <- zip xs (tail xs)]

indices :: Eq a => a -> [a] -> [Int]
indices x xs = [i | (i, v) <- zip [0 ..] xs, v == x]

alphabet :: Int -> String
alphabet n = take n [c | c <- ['a' ..]]
