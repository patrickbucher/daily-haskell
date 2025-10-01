import Data.Char

data Player
  = X
  | O
  deriving (Eq, Show)

type Field = Maybe Player

type Grid = [[Field]]

type Pos = (Int, Int)

type Change = (Int, Int, Player)

type Move = (Pos, Pos)

side :: Int
side = 7

initial :: Grid
initial =
  applyChanges
    (replicate side (replicate side Nothing))
    [ (0, 0, X)
    , (0, (side - 1), O)
    , ((side - 1), 0, O)
    , ((side - 1), (side - 1), X)
    ]

applyChanges :: Grid -> [Change] -> Grid
applyChanges g ms =
  chop
    side
    [ if (r, c) `elem` xs
      then Just X
      else if (r, c) `elem` os
             then Just O
             else g !! r !! c
    | r <- [0 .. side - 1]
    , c <- [0 .. side - 1]
    ]
  where
    moves p ms = map (\(r, c, _) -> (r, c)) $ filter (\(_, _, v) -> v == p) ms
    xs = moves X ms
    os = moves O ms

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
