module Main where

foldLeft f acc xs =
  if null xs
  then acc
  else foldLeft f (f acc (head xs)) (tail xs)

foldRight f acc xs =
  if null xs
  then acc
  else f (head xs) $ foldRight f acc (tail xs)

main =
  -- print $ foldLeft (<>) "" ["a","b","c"]
  print $ foldRight (<>) "" ["a","b","c"]
