import Data.Char

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
    -- TODO: refactor this mess
    [ if (r, c) `elem` xs
      then Just X
      else if (r, c) `elem` os
             then Just O
             else if (r, c) `elem` bs
                    then Nothing
                    else g !! r !! c
    | r <- [0 .. side - 1]
    , c <- [0 .. side - 1]
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
displayGrid g = concat [r ++ "\n" | r <- colcaps : rows ++ [colcaps]]
  where
    colcaps = "  " ++ take side ['a' ..]
    rowcaps = take side ['1' ..]
    rows = map (\(n, r) -> n : ' ' : (displayRow r) ++ [' ', n]) (zip rowcaps g)
    displayRow r = map displayField r

displayField :: Field -> Char
displayField (Just X) = 'X'
displayField (Just O) = 'O'
displayField Nothing = '-'

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

applyMove :: Grid -> Player -> Move -> Grid
applyMove g p m = applyChanges g changes
  where
    jump ((r, c), (r', c')) = abs (r - r') == 2 && abs (c - c') == 2
    toChange (r, c) f = (r, c, f)
    -- TODO: consider changes made to neighbouring opponent fields
    changes =
      if jump m
        then [toChange (fst m) Nothing, toChange (snd m) (Just p)]
        else [toChange (snd m) (Just p)]

neighbourCoords :: Grid -> Pos -> [Pos]
neighbourCoords g (r, c) =
  filter (\(r, c) -> r `elem` range && c `elem` range) neighbours
  where
    dirs = [(r, c) | r <- [-1, 0, 1], c <- [-1, 0, 1], r /= 0 || c /= 0]
    neighbours = map (\(dr, dc) -> (r + dr, c + dc)) dirs
    range = take side [0 ..]
