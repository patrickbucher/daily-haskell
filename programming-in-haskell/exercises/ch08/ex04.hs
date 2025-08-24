data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

halves :: [a] -> ([a], [a])
halves xs = (take n xs, drop n xs)
  where
    n = (length xs) `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
  where
    (ys, zs) = halves xs
