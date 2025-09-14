import Control.Concurrent
import Data.Char
import Data.List
import System.IO
import System.Random (randomRIO)

data Player
  = X
  | O
  | E
  deriving (Eq)

instance Show Player where
  show X = "X" -- black
  show O = "O" -- white
  show E = "-" -- empty

type Grid = [[Player]]

type Pos = (Int, Int)

type Shift = Int -> Int

data Tree
  = Node Grid (Maybe Int) Player [(Pos, Tree)]
  | Leaf Grid (Maybe Int)
  deriving (Show)

directions :: [(Shift, Shift)]
directions =
  [ (subtract 1, id) -- north
  , (subtract 1, (+ 1)) -- north-east
  , (id, (+ 1)) -- east
  , ((+ 1), (+ 1)) -- south-east
  , ((+ 1), id) -- south
  , ((+ 1), subtract 1) -- south-west
  , (id, subtract 1) -- west
  , (subtract 1, subtract 1) -- north-west
  ]

initial :: Grid
initial =
  [ [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, O, X, E, E, E]
  , [E, E, E, X, O, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  ]

opponent :: Player -> Player
opponent X = O
opponent O = X
opponent E = E

paths :: Grid -> Pos -> [[Player]]
paths g pos = map (follow g pos) directions

follow :: Grid -> Pos -> (Shift, Shift) -> [Player]
follow g (r, c) (dr, dc) =
  [ g !! r' !! c'
  | (r', c') <-
      zip
        (takeWhile (\i -> i `elem` [0 .. 7]) (iterate dr r))
        (takeWhile (\i -> i `elem` [0 .. 7]) (iterate dc c))
  ]

validMove :: Grid -> Pos -> Player -> Bool
validMove g (r, c) p = (length validPaths) > 0
  where
    allPaths = paths g (r, c)
    validPaths = filter (projectsStraight p) allPaths

projectsStraight :: Player -> [Player] -> Bool
projectsStraight p path = startsEmpty && gapsOpposite && reachesOwn
  where
    op = opponent p
    startsEmpty = (head path) == E
    gapsOpposite = length (takeWhile (== op) (tail path)) > 0
    remainder = dropWhile (== op) (tail path)
    reachesOwn =
      if not (null remainder)
        then (head remainder) == p
        else False

applyMove :: Grid -> Pos -> Player -> Grid
applyMove g (r, c) p = applyChanges g coords p
  where
    perDirection =
      map (\shifts -> affectedCoordinates g (r, c) p shifts []) directions
    coords = concat perDirection

affectedCoordinates :: Grid -> Pos -> Player -> (Shift, Shift) -> [Pos] -> [Pos]
affectedCoordinates g (r, c) p (dr, dc) acc =
  case (f, acc) of
    (Nothing, _) -> []
    (Just E, []) -> affectedCoordinates g (r', c') p (dr, dc) [(r, c)]
    (Just E, _) -> []
    (Just p', []) -> []
    (Just p', (_:cs))
      | p' == op -> affectedCoordinates g (r', c') p (dr, dc) ((r, c) : acc)
      | p' == p && null cs -> []
      | otherwise -> reverse acc
  where
    f =
      if r `elem` [0 .. 7] && c `elem` [0 .. 7]
        then Just (g !! r !! c)
        else Nothing
    r' = dr r
    c' = dc c
    op = opponent p

applyChanges :: Grid -> [Pos] -> Player -> Grid
applyChanges g coords p =
  chop
    [ if ((r, c) `elem` coords)
      then p
      else (g !! r !! c)
    | r <- [0 .. rows - 1]
    , c <- [0 .. cols - 1]
    ]
    rows
  where
    rows = length g
    cols = length (g !! 0)

chop :: [a] -> Int -> [[a]]
chop [] _ = []
chop xs n = take n xs : chop (drop n xs) n

score :: Player -> Grid -> Int
score p = length . filter (== p) . concat

display :: Grid -> String
display g = concat ((title : rowsCaptioned) ++ [title] ++ ["\n", standings])
  where
    standings =
      show X
        ++ " "
        ++ (show (score X g))
        ++ ":"
        ++ (show (score O g))
        ++ " "
        ++ show O
    title = "  " ++ (foldl1 (++) $ (map show [1 .. 8])) ++ "\n"
    rows = zip ['a' ..] (map concat $ map (map show) g)
    rowsCaptioned = map (\(c, r) -> [c, ' '] ++ r ++ [' ', c, '\n']) rows

parseMove :: String -> Maybe Pos
parseMove [r, c] =
  if r' `elem` [0 .. 7] && c' `elem` [0 .. 7]
    then Just (r', c')
    else Nothing
  where
    r' = ord r - ord 'a'
    c' = (digitToInt c) - 1
parseMove _ = Nothing

displayMove :: Pos -> String
displayMove (r, c) = [chr (r + ord 'a'), chr (c + ord '1')]

finished :: Grid -> Maybe Player
finished g =
  if full || blocked
    then if diffXO == 0
           then Just E
           else if diffXO > 0
                  then Just X
                  else Just O
    else Nothing
  where
    stones = concat g
    full = all (/= E) stones
    diffXO = score X g - score O g
    blocked = (null $ possibleMoves g X) && (null $ possibleMoves g O)

promptMove :: Grid -> Player -> IO Pos
promptMove g p = do
  putStr $ "Player " ++ show p ++ ": "
  input <- getLine
  case (parseMove input) of
    Just (r, c) ->
      if validMove g (r, c) p
        then return (r, c)
        else do
          putStrLn ""
          promptMove g p
    Nothing -> promptMove g p

aiMoveFunc :: Int -> Grid -> Player -> IO Pos
aiMoveFunc n = (\g p -> aiMove n g p)

aiMove :: Int -> Grid -> Player -> IO Pos
aiMove d g p = do
  i <- randomRIO (0, (length moves) - 1)
  let move = moves !! i
  putStr $ "Player " ++ show p ++ ": "
  threadDelay $ floor (2_000_000 * delayFactor)
  putStr $ displayMove move
  threadDelay 1_000_000
  return move
  where
    tree = buildTree g p d
    moves = bestMoves tree
    delayFactor = (1.0 - fromIntegral d / 9.0)

size :: Tree -> Int
size (Leaf _ _) = 1
size (Node _ _ _ []) = 1
size (Node _ _ _ xs) = 1 + sum (map size (map snd xs))

buildTree :: Grid -> Player -> Int -> Tree
buildTree g p 0 = Leaf g (Just value)
  where
    value = score X g - score O g
buildTree g p n = Node g (Just $ value (Node g Nothing p children)) p children
  where
    children =
      [ (m, buildTree (applyMove g m p) (opponent p) (n - 1))
      | m <- possibleMoves g p
      , (finished g) == Nothing
      ]
    optimize
      | p == X = max
      | p == O = min
    value (Leaf g (Just v)) = v
    value (Node g (Just v) p cs) = v
    value (Node g Nothing _ []) = score X g - score O g
    value (Node g Nothing p cs) = foldl1 optimize (map value (map snd cs))

bestMoves :: Tree -> [Pos]
bestMoves (Node g v p []) = []
bestMoves (Node g v p ns) = map fst moves
  where
    eval (pos, Leaf _ (Just v)) = (pos, v)
    eval (pos, Node _ (Just v) _ _) = (pos, v)
    outcomes = map eval ns
    results = map snd outcomes
    optimize
      | p == X = max
      | p == O = min
    optimal = foldl1 optimize results
    moves = filter (\(pos, outcome) -> outcome == optimal) outcomes

possibleMoves :: Grid -> Player -> [Pos]
possibleMoves g p = filter (\pos -> validMove g pos p) candidates
  where
    candidates = [(r, c) | r <- [0 .. 7], c <- [0 .. 7], g !! r !! c == E]

promptColor :: IO Player
promptColor = do
  cls
  putStr
    $ "Do you want to play as "
        ++ show X
        ++ " (first) or as "
        ++ show O
        ++ " (second)? "
  input <- getLine
  case (input) of
    "X" -> return X
    "O" -> return O
    _ -> promptColor

promptAiLevel :: IO Int
promptAiLevel = do
  cls
  putStr $ "How smart shall the opponent be (1-9)? "
  input <- getLine
  if input `elem` map show [1 .. 9]
    then return (read input)
    else promptAiLevel

cls :: IO ()
cls = putStr "\ESC[2J\ESC[0;0H"

scoreStr :: Grid -> Player -> String
scoreStr g p = show (score p g) ++ ":" ++ show (score (opponent p) g)

play :: Int -> Grid -> (Player, Player) -> Player -> IO Grid
play l g (h, c) p = do
  cls
  putStrLn $ display g
  case (finished g) of
    Just E -> do
      putStrLn ("Draw " ++ scoreStr g p)
      return g
    Just p -> do
      putStrLn ("Player " ++ show p ++ " wins " ++ scoreStr g p)
      return g
    Nothing -> do
      putStrLn
        (if p /= p'
           then show p ++ " can't move, skipping… "
           else "")
      move <- f g p'
      let g' = applyMove g move p'
      play l g' (h, c) (opponent p')
  where
    (f, p') =
      case (null (possibleMoves g p), p) of
        (False, p)
          | p == h -> (promptMove, h)
          | p == c -> (aiMoveFunc l, c)
        (True, p)
          | p == h -> (aiMoveFunc l, c)
          | p == c -> (promptMove, h)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  human <- promptColor
  level <- promptAiLevel
  let computer = opponent human
  let begins =
        if human == X
          then human
          else computer
  _ <- play level initial (human, computer) begins
  return ()
