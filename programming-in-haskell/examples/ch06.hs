factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

sum :: Num a => [a] -> a
sum [] = 0
sum (n:ns) = n + Main.sum ns

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : Main.zip xs ys

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

even :: Int -> Bool
even 0 = True
even n = odd (n - 1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n - 1)
