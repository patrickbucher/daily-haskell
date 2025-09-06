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
  [ (id, (subtract 1)) -- north
  , ((+ 1), (subtract 1)) -- north-east
  , ((+ 1), id) -- east
  , ((+ 1), (+ 1)) -- south-easth
  , (id, (+ 1)) -- south
  , ((subtract 1), (+ 1)) -- south-west
  , ((subtract 1), id) -- west
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
  [ g !! y !! x
  | (x, y) <-
      zip
        (takeWhile (\i -> i `elem` [0 .. 7]) (iterate dy r))
        (takeWhile (\i -> i `elem` [0 .. 7]) (iterate dx c))
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

affectedCoordinates :: Grid -> Pos -> Player -> (Shift, Shift) -> [Pos] -> [Pos]
affectedCoordinates g (r, c) p (dx, dy) acc =
  case (field, acc) of
    (Just E, []) -> affectedCoordinates g (dy r, dx c) p (dx, dy) [(r, c)]
    (Just E, _) -> []
    (Just opp, []) -> []
    (Just opp, _) ->
      affectedCoordinates g (dy r, dx c) p (dx, dy) ((r, c) : acc)
    (Just p, []) -> []
    (Just p, [_]) -> []
    (Just p, _) -> acc
    _ -> []
  where
    field =
      if c `elem` [0 .. 7] && r `elem` [0 .. 7]
        then Just (g !! r !! c)
        else Nothing
    opp = opponent p
    valid = acc == [] && field == Just E || length acc > 0 && field == Just opp
    closing = length acc > 0 && field == Just p

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
