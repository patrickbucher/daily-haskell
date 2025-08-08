qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
  where
    larger = [a | a <- xs, a > x]
    smaller = [b | b <- xs, b <= x]
