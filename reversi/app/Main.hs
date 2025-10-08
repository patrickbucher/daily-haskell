module Main where

import Control.Concurrent
import Control.Parallel
import Data.Char
import Data.List
import System.IO
import System.Random (randomRIO)

import Reversi.Board
  ( Grid
  , Player(..)
  , Pos
  , applyMove
  , display
  , initial
  , opponent
  , possibleMoves
  , score
  , sides
  , validMove
  )

import Reversi.Minimax (bestMoves, buildTree, buildTreeAB)

parseMove :: String -> Maybe Pos
parseMove [r, c] =
  if r' `elem` [0 .. sides - 1] && c' `elem` [0 .. sides - 1]
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
    Nothing ->
      case input of
        ('H':l) ->
          if all isNumber l
            then do
              putStrLn $ showHints g p (read l :: Int)
              promptMove g p
            else promptMove g p
        _ -> promptMove g p

showHints :: Grid -> Player -> Int -> String
showHints g p l =
  foldl (++) "" $ intersperse ", " $ map displayMove $ map fst movesOrdered
  where
    fields = sides ^ 2
    tree = buildTree g p l
    moves = bestMoves tree
    moveResults = map (\m -> (m, applyMove g m p)) moves
    moveScores =
      map (\(m, g) -> (m, (score p g) - (score (opponent p) g))) moveResults
    movesOrdered = reverse $ sortBy (\l r -> compare (snd l) (snd r)) moveScores

aiMoveFunc :: Int -> Grid -> Player -> IO Pos
aiMoveFunc n = (\g p -> aiMove n g p)

aiMove :: Int -> Grid -> Player -> IO Pos
aiMove d g p = do
  i <- randomRIO (0, (length moves) - 1)
  let move = moves !! i
  putStr $ "Player " ++ show p ++ ": "
  threadDelay 1000000
  putStr $ displayMove move
  threadDelay 1000000
  return move
  where
    fields = sides ^ 2
    tree = buildTreeAB g p d (-fields, fields)
    moves = bestMoves tree

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
