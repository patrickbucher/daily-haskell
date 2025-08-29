adder :: IO ()
adder = do
  putStrLn "How many numbers? "
  s <- getLine
  ss <- sequence (replicate (read s :: Int) getLine)
  putStrLn (show (foldl (+) 0 (map (\s -> (read s :: Int)) ss)))
