import Control.Concurrent (threadDelay)
import Data.Char
import System.Random (randomRIO)

data Player
  = X
  | O
  deriving (Eq, Show)

type Field = Maybe Player

type Grid = [[Field]]

type Pos = (Int, Int)

type Change = (Int, Int, Field)

type Move = (Pos, Pos)

side :: Int
side = 7

initial :: Grid
initial =
  applyChanges
    (replicate side (replicate side Nothing))
    [ (0, 0, Just X)
    , (0, (side - 1), Just O)
    , ((side - 1), 0, Just O)
    , ((side - 1), (side - 1), Just X)
    ]

applyChanges :: Grid -> [Change] -> Grid
applyChanges g ms =
  chop
    side
    [ case (m `elem` xs, m `elem` os, m `elem` bs) of
      (True, _, _) -> Just X
      (_, True, _) -> Just O
      (_, _, True) -> Nothing
      _ -> g !! r !! c
    | r <- [0 .. side - 1]
    , c <- [0 .. side - 1]
    , let m = (r, c)
    ]
  where
    moves p ms = map (\(r, c, _) -> (r, c)) $ filter (\(_, _, v) -> v == p) ms
    xs = moves (Just X) ms
    os = moves (Just O) ms
    bs = moves Nothing ms

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

displayGrid :: Grid -> String
displayGrid g =
  concat [r ++ "\n" | r <- (displayScore g) : "" : colcaps : rows ++ [colcaps]]
  where
    colcaps = "  " ++ take side ['a' ..]
    rowcaps = take side ['1' ..]
    rows = map (\(n, r) -> n : ' ' : (displayRow r) ++ [' ', n]) (zip rowcaps g)
    displayRow r = map displayField r

displayField :: Field -> Char
displayField (Just X) = 'X'
displayField (Just O) = 'O'
displayField Nothing = '-'

displayScore :: Grid -> String
displayScore g =
  (show X) ++ "  " ++ (scoreStr X) ++ ":" ++ (scoreStr O) ++ "  " ++ (show O)
  where
    scoreStr p = zeroPad (show $ score p) 2
    score p = length $ filter (== (Just p)) $ concat g

zeroPad :: String -> Int -> String
zeroPad s n =
  if l >= n
    then s
    else take (n - l) (repeat '0') ++ s
  where
    l = length s

parseMove :: String -> Maybe Move
parseMove [x, y, ' ', x', y'] =
  if ok
    then Just ((r, c), (r', c'))
    else Nothing
  where
    rows = take side ['1' ..]
    cols = take side ['a' ..]
    ok = x `elem` cols && y `elem` rows && x' `elem` cols && y' `elem` rows
    r = ord y - ord '1'
    c = ord x - ord 'a'
    r' = ord y' - ord '1'
    c' = ord x' - ord 'a'
parseMove _ = Nothing

renderMove :: Move -> String
renderMove ((r, c), (r', c')) = [x, y, ' ', x', y']
  where
    x = chr (c + ord 'a')
    y = chr (r + ord '1')
    x' = chr (c' + ord 'a')
    y' = chr (r' + ord '1')

isValid :: Grid -> Player -> Move -> Bool
isValid g p ((r, c), (r', c')) = properSource && onGrid && withinRange
  where
    properSource = g !! r !! c == Just p
    onGrid = r `elem` idx && c `elem` idx && r' `elem` idx && c' `elem` idx
    withinRange = abs (r' - r) <= 2 && abs (c' - c) <= 2
    idx = take side [0 ..]

opponent :: Player -> Player
opponent X = O
opponent O = X

applyMove :: Grid -> Player -> Move -> Grid
applyMove g p m = applyChanges g changes
  where
    jump ((r, c), (r', c')) = abs (r - r') == 2 || abs (c - c') == 2
    toChange (r, c) f = (r, c, f)
    neighbours = neighbourCoords g (snd m)
    caught = filter (\(r, c) -> (g !! r !! c) == Just (opponent p)) neighbours
    changes =
      map (\m -> toChange m (Just p)) caught
        ++ if jump m
             then [toChange (fst m) Nothing, toChange (snd m) (Just p)]
             else [toChange (snd m) (Just p)]

neighbourCoords :: Grid -> Pos -> [Pos]
neighbourCoords g (r, c) =
  filter (\(r, c) -> r `elem` range && c `elem` range) neighbours
  where
    dirs = [(r, c) | r <- [-1, 0, 1], c <- [-1, 0, 1], r /= 0 || c /= 0]
    neighbours = map (\(dr, dc) -> (r + dr, c + dc)) dirs
    range = take side [0 ..]

validMoves :: Grid -> Player -> Pos -> [Move]
validMoves g p (r, c) =
  case field of
    Just p'
      | p' == p -> map (\(r', c') -> ((r, c), (r', c'))) onEmpty
      | otherwise -> []
    Nothing -> []
  where
    field = g !! r !! c
    onEmpty = filter (\(r', c') -> g !! r' !! c' == Nothing) onGrid
    onGrid = filter (\(r', c') -> r' `elem` limits && c' `elem` limits) coords
    limits = take side [0 ..]
    coords = map (\(r', c') -> (r + r', c + c')) shifts
    shifts = [(r', c') | r' <- [-2 .. 2], c' <- [-2 .. 2]]

cls :: IO ()
cls = putStr "\ESC[2J\ESC[0;0H"

pickPlayer :: IO Player
pickPlayer = do
  cls
  putStrLn
    $ "Would you like to play as "
        ++ show X
        ++ " (first) or as "
        ++ show O
        ++ " (second)?"
  input <- getLine
  case input of
    "X" -> return X
    "O" -> return O
    otherwise -> pickPlayer

promptMove :: Grid -> Player -> IO Move
promptMove g p = do
  putStr $ show p ++ ": "
  input <- getLine
  case (parseMove input) of
    Just (s, d) ->
      case ((s, d) `elem` validMoves g p s) of
        True -> return (s, d)
        False -> promptMove g p
    Nothing -> promptMove g p

randomMove :: Grid -> Player -> IO Move
randomMove g p = do
  i <- randomRIO (0, (length moves) - 1)
  putStr $ show p ++ ": "
  threadDelay 1_000_000
  let move = moves !! i
  putStr $ renderMove $ move
  threadDelay 1_000_000
  return $ move
  where
    moves = concat $ map (\(r', c') -> validMoves g p (r', c')) fields
    fields =
      [ (r', c')
      | r' <- take side [0 ..]
      , c' <- take side [0 ..]
      , g !! r' !! c' == Just p
      ]

-- TODO: check for win, draw, stuck game etc.
play :: Grid -> Player -> Player -> IO Player
play g p human = do
  cls
  putStrLn $ displayGrid g
  m <-
    if p == human
      then do
        promptMove g p
      else do
        randomMove g p
  play (applyMove g p m) (opponent p) human
  where
    opponent X = O
    opponent O = X

main :: IO ()
main = do
  player <- pickPlayer
  play initial X player
  return ()
