module Main where

fizzbuzz n =
  if n == 0
  then ""
  else fizzbuzz (n - 1) <> "\n" <> denote n
  where
    denote n
      | n `rem` 15 == 0 = "FizzBuzz"
      | n `rem` 5 == 0 = "Buzz"
      | n `rem` 3 == 0 = "Fizz"
      | otherwise = show n

main = print $ fizzbuzz 100
