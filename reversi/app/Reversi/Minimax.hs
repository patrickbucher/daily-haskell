module Reversi.Minimax
  ( Tree
  , buildTree
  , buildTreeAB
  , bestMoves
  , possibleMoves
  ) where

import Control.Parallel

import Reversi.Board
  ( Grid
  , Player(..)
  , Pos
  , applyMove
  , opponent
  , possibleMoves
  , score
  , sides
  , validMove
  )

data Tree =
  Node Grid Int Player [(Pos, Tree)]
  deriving (Show)

leaves :: Tree -> Int
leaves (Node _ _ _ []) = 1
leaves (Node _ _ _ cs) = foldl (+) 0 $ map leaves (map snd cs)

buildTree :: Grid -> Player -> Int -> Tree
buildTree g p 0 = Node g value p []
  where
    value = score X g - score O g
buildTree g p n = Node g value p children
  where
    children =
      [ (m, buildTree (applyMove g m p) (opponent p) (n - 1))
      | m <- possibleMoves g p
      ]
    fields = sides ^ 2
    (worst, optimize)
      | p == X = (-fields, max)
      | p == O = (fields, min)
    value = foldl optimize worst $ map (\(_, Node _ v _ _) -> v) children

buildTreeAB :: Grid -> Player -> Int -> (Int, Int) -> Tree
buildTreeAB g p 0 _ = Node g value p []
  where
    value = score X g - score O g
buildTreeAB g p n (a, b) = Node g value p children
  where
    (children, _) = buildChildren g p n (possibleMoves g p) (a, b)
    fields = sides ^ 2
    (worst, optimize)
      | p == X = (-fields, max)
      | p == O = (fields, min)
    eval (Node _ v _ _) = v
    value = foldl optimize worst $ map eval (map snd children)

buildChildren ::
     Grid -> Player -> Int -> [Pos] -> (Int, Int) -> ([(Pos, Tree)], (Int, Int))
buildChildren _ _ _ [] ab = ([], ab)
buildChildren g p n (m:ms) (a, b) =
  children `par` siblings `pseq` (result, (alpha, beta))
  where
    g' = applyMove g m p
    (Node _ _ _ children) = buildTreeAB g' (opponent p) (n - 1) (a, b)
    node = Node g' value p children
    (siblings, _) = buildChildren g p n ms (alpha, beta)
    result
      | quit = []
      | otherwise = (m, node) : siblings
    eval (Node _ v _ _) = v
    fields = sides ^ 2
    (worst, optimize)
      | p == X = (-fields, max)
      | p == O = (fields, min)
    value = foldl optimize worst $ map eval (map snd children)
    alpha
      | p == X = max value a
      | otherwise = a
    beta
      | p == O = min value b
      | otherwise = b
    quit
      | p == X = value > beta
      | p == O = value < alpha

bestMoves :: Tree -> [Pos]
bestMoves (Node g v p ns) = map fst moves
  where
    eval (pos, Node _ v _ _) = [(pos, v)]
    outcomes = concat $ map eval ns
    results = map snd outcomes
    fields = sides ^ 2
    (worst, optimize)
      | p == X = (-fields, max)
      | p == O = (fields, min)
    optimal = foldl optimize worst results
    moves = filter (\(pos, outcome) -> outcome == optimal) outcomes
