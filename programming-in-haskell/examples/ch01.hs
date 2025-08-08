double :: Num a => a -> a
double x = x + x

sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (n:ns) = n + sum2 ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) =
    do
        x <- act
        xs <- seqn acts
        return (x:xs)
        

