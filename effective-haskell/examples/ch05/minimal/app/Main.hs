module Main where
import Prelude hiding (length)

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

main = print $ length "Hello, World!"
