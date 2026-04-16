module Main where

fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | n == 2 = 1
  | otherwise = fibonacci (n - 2) + fibonacci (n - 1)

main = print $ fibonacci 25
