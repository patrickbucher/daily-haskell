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
    reachesOwn = head (dropWhile (== op) (tail path)) == p

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
