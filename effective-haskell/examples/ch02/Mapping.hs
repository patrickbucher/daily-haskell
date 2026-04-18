module Main where

map' f xs =
  if null xs
  then []
  else f (head xs) : map' f (tail xs)

main =
  print $ map' (*2) [1..5]
