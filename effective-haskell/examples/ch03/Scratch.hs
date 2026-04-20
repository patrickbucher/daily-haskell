module Main where

hypot :: Float -> Float -> Float
hypot a b = sqrt $ a^2 + b^2

main =
  print $ hypot 3 4
