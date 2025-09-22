import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player
  = O
  | B
  | X
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g =
  if os <= xs
    then O
    else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

won :: Grid -> Bool
won g = wins O g || wins X g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

putGrid :: Grid -> IO ()
putGrid = putStrLn . concat . interleave "\n" . map showRow

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> String
showRow = foldr1 (++) . interleave "|" . map showPlayer

showPlayer :: Player -> String
showPlayer B = " "
showPlayer x = show x

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> Grid
move g i p = (chop size (xs ++ [p] ++ ys))
  where
    (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

data Tree a =
  Node a [(Int, Tree a)]
  deriving (Show)

main :: IO ()
main = do
  startingPlayer <- promptFirst
  hSetBuffering stdout NoBuffering
  play empty startingPlayer (buildTree depth startingPlayer empty)
  where
    depth = size ^ 2

promptFirst :: IO Player
promptFirst = do
  putStrLn "Which player shall begin? X or O?"
  pick <- getLine
  case (pick !! 0) of
    'O' -> return O
    'X' -> return X
    otherwise -> promptFirst

play :: Grid -> Player -> Tree (Grid, Maybe Player) -> IO ()
play g p t = do
  cls
  goto (1, 1)
  putGrid g
  play' g p t

play' :: Grid -> Player -> Tree (Grid, Maybe Player) -> IO ()
play' g p t
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    m <- getNat (prompt p)
    if valid g m
      then play (move g m p) (next p) (descend t m)
      else do
        putStrLn "ERROR: Invalid move"
        play' g p t
  | p == X = do
    play (move g m p) (next p) (descend t m)
  where
    m = fst $ head $ bestMoves X t

leaves :: Tree a -> Int
leaves (Node _ []) = 1
leaves (Node _ cs) = foldl (+) 0 $ map leaves (map snd cs)

descend :: Tree a -> Int -> Tree a
descend (Node _ cs) m = snd $ head $ filter (\(m', t) -> m' == m) cs

buildTreeAB ::
     Int
  -> Player
  -> Grid
  -> (Int, Int)
  -> (Tree (Grid, Maybe Player), (Int, Int))
buildTreeAB 0 _ g ab = (Node (g, winner g) [], ab)
buildTreeAB n p g ab = (Node (g, winner g) children, ab)
  where
    terminal = winner g /= Nothing
    (children, ab)
      | terminal = ([], ab)
      | otherwise = buildChildren (n - 1) (next p) g (possibleMoves g) ab

buildChildren ::
     Int
  -> Player
  -> Grid
  -> [Int]
  -> (Int, Int)
  -> ([(Int, Tree (Grid, Maybe Player))], (Int, Int))
buildChildren d p g [] ab = ([], ab)
buildChildren d p g (m:ms) (a, b) = ([], (a, b))
  where
    g' = move g m p
    (node, (alpha, beta)) = buildNodeAB d p g' (a, b)
    (siblings, _) = buildChildren d p g ms (alpha, beta)
    result = (node : map snd siblings)
    value (Node (_, w) _) = rate w

buildNodeAB ::
     Int
  -> Player
  -> Grid
  -> (Int, Int)
  -> (Tree (Grid, Maybe Player), (Int, Int))
buildNodeAB n p g ab = (Node (g, winner g) children, (alpha, beta))
  where
    (Node _ children, (alpha, beta)) = buildTreeAB (n - 1) (next p) g ab

buildTree :: Int -> Player -> Grid -> Tree (Grid, Maybe Player)
buildTree 0 _ g = Node (g, winner g) []
buildTree n p g = Node (g, winner g) children
  where
    terminal = winner g /= Nothing
    children
      | terminal = []
      | otherwise = [(m, buildNode n p g m) | m <- possibleMoves g]

buildNode :: Int -> Player -> Grid -> Int -> Tree (Grid, Maybe Player)
buildNode n p g m = Node (g', winner g') children
  where
    g' = move g m p
    (Node _ children) = buildTree (n - 1) (next p) g'

winner :: Grid -> Maybe Player
winner g
  | wins X g = Just X
  | wins O g = Just O
  | full g = Just B
  | otherwise = Nothing

possibleMoves :: Grid -> [Int]
possibleMoves g = [i | i <- [0 .. (size ^ 2) - 1], (concat g) !! i == B]

bestMoves :: Player -> Tree (Grid, Maybe Player) -> [(Int, Int)]
bestMoves p (Node _ []) = []
bestMoves p (Node _ cs) = sorted
  where
    order
      | p == X = reverse
      | p == O = id
    moves = map (\(m, c) -> (m, outcome (next p) (m, c))) cs
    sorted = order $ sortBy (\(_, l) (_, r) -> compare l r) moves

outcome :: Player -> (Int, Tree (Grid, Maybe Player)) -> Int
outcome p (_, (Node (g, w) [])) = rate w
outcome p (_, (Node (g, w) cs)) =
  if w == Nothing
    then best
    else rate w
  where
    order
      | p == X = reverse
      | p == O = id
    subs = map (\(m, t) -> (m, outcome (next p) (m, t))) $ cs
    sorted = order $ sortBy (\(_, l) (_, r) -> compare l r) subs
    best = snd $ head sorted

rate :: Maybe Player -> Int
rate (Just X) = 1
rate (Just B) = 0
rate Nothing = 0
rate (Just O) = -1
