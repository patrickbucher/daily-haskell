import Data.List

type Grid = [[Player]]

size :: Int
size = 3

data Player
  = O
  | B
  | X
  deriving (Eq, Ord, Show)

data Tree a =
  Node a [Tree a]
  deriving (Show)

-- > treesize $ gametree empty O
-- 740170
-- > treesize $ gametree empty X
-- 740170
treesize :: Tree a -> Int
treesize (Node _ []) = 1
treesize (Node _ children) = 1 + sum (map treesize children)

-- > depth $ gametree empty O
-- 10
-- > depth $ gametree empty X
-- 10
depth :: Tree a -> Int
depth (Node _ []) = 1
depth (Node _ children) = 1 + maximum (map depth children)

empty :: Grid
empty = replicate size (replicate size B)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

next :: Player -> Player
next O = X
next B = B
next X = O

moves :: Grid -> Player -> [Grid]
moves g p
  | wins p g = []
  | full g = []
  | otherwise = concat [maybeToList (move g i p) | i <- [0 .. ((size ^ 2) - 1)]]
  where
    maybeToList (Just x) = [x]
    maybeToList Nothing = []

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

full :: Grid -> Bool
full = all (/= B) . concat

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> Maybe Grid
move g i p =
  if valid g i
    then Just (chop size (xs ++ [p] ++ ys))
    else Nothing
  where
    (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
