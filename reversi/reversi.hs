data Player
  = X
  | O
  | E
  deriving (Eq)

instance Show Player where
  show X = "X"
  show O = "O"
  show E = "-"

type Grid = [[Player]]

type Pos = (Int, Int)

type Shift = Int -> Int

directions :: [(Shift, Shift)]
directions =
  [ ((+ 0), (subtract 1)) -- north
  , ((+ 1), (subtract 1)) -- north-east
  , ((+ 1), (+ 0)) -- east
  , ((+ 1), (+ 1)) -- south-easth
  , ((+ 0), (+ 1)) -- south
  , ((subtract 1), (+ 1)) -- south-west
  , ((subtract 1), (+ 0)) -- west
  , ((subtract 1), (subtract 1)) -- north-west
  ]

initial :: Grid
initial =
  [ [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, X, O, E, E, E]
  , [E, E, E, O, X, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  , [E, E, E, E, E, E, E, E]
  ]

opponent :: Player -> Player
opponent X = O
opponent O = X
opponent E = E

display :: Grid -> String
display g = concat [concat ((map show ps) ++ ["\n"]) | ps <- g]

paths :: Grid -> Pos -> [[Player]]
paths g pos = map (follow g pos) directions

follow :: Grid -> Pos -> (Shift, Shift) -> [Player]
follow g (r, c) (dx, dy) =
  [ g !! x !! y
  | (x, y) <-
      zip
        (takeWhile (\x -> x `elem` [0 .. 7]) (iterate dx c))
        (takeWhile (\y -> y `elem` [0 .. 7]) (iterate dy r))
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
    reachesOwn = head (dropWhile (== op) (tail path)) == p

-- idea: recursive function taking a Grid, a Pos, a Shift
-- returns affected coords
applyMove :: Grid -> Pos -> Player -> Grid
applyMove g (r, c) p = g -- FIXME

-- TODO: Given a grid, a position, and a direction (two shifts), and a player,
-- write a recursive function that returns the coordinates of the fields that
-- need to be set to that player.

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
