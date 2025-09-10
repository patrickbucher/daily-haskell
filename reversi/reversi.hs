import Control.Concurrent
import Data.Char
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

diff :: Player -> Grid -> Int
diff p g = score p g - score (opponent p) g

display :: Grid -> String
display g = concat ((title : rowsCaptioned) ++ ["", standings])
  where
    standings =
      show X
        ++ " "
        ++ (show (score X g))
        ++ ":"
        ++ (show (score O g))
        ++ " "
        ++ show O
        ++ "\n"
    title = "  " ++ (foldl1 (++) $ (map show [1 .. 8])) ++ "\n"
    rows = zip ['a' ..] (map concat $ map (map show) g)
    rowsCaptioned = map (\(c, r) -> [c, ' '] ++ r ++ ['\n']) rows

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
    Nothing -> promptMove g p -- FIXME: if there are no possible move, return Nothing (change type)

randomMove :: Grid -> Player -> IO (Maybe Pos)
randomMove g p = do
  i <- randomRIO (0, length moves - 1)
  return
    (if (i < 0)
       then Nothing
       else Just (moves !! i))
  where
    candidates = [(r, c) | r <- [0 .. 7], c <- [0 .. 7], g !! r !! c == E]
    moves = filter (\pos -> validMove g pos p) candidates

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

cls :: IO ()
cls = putStr "\ESC[2J\ESC[0;0H"

-- TODO: handle win/draw condition, skip player if there are no valid moves for him
-- TODO: consider displaying valid moves
play :: Grid -> (Player, Player) -> Player -> IO Grid
play g (h, c) p
  | p == h = do
    cls
    putStrLn $ display g
    move <- promptMove g p
    play (applyMove g move p) (h, c) (opponent p)
  | p == c = do
    cls
    putStrLn $ display g
    move <- randomMove g p
    case move of
      Just m -> do
        putStr $ "Player O: "
        threadDelay 2_000_000
        putStr $ displayMove m
        threadDelay 1_000_000
        play (applyMove g m p) (h, c) (opponent p)
      Nothing -> return g

main :: IO ()
main = do
  human <- promptColor
  let computer = opponent human
  let begins =
        if human == X
          then human
          else computer
  _ <- play initial (human, computer) begins
  return ()
