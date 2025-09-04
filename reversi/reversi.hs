data Player
  = X
  | O
  | E

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

display :: Grid -> String
display g = concat [concat ((map show ps) ++ ["\n"]) | ps <- g]

paths :: Grid -> Pos -> [[Player]]
paths g pos = map (follow pos g) directions

follow :: Pos -> Grid -> (Shift, Shift) -> [Player]
follow (r, c) g (dx, dy) =
  [ g !! x !! y
  | (x, y) <-
      zip
        (takeWhile (\x -> x `elem` [0 .. 7]) (iterate dx c))
        (takeWhile (\y -> y `elem` [0 .. 7]) (iterate dy r))
  ]
