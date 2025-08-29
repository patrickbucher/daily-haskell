adder :: IO ()
adder = do
  putStrLn "How many numbers? "
  s <- getLine
  sumN ((read s) :: Int) 0

sumN :: Int -> Int -> IO ()
sumN 0 acc = do
  putStrLn (show acc)
sumN n acc = do
  s <- getLine
  sumN (n - 1) (acc + ((read s) :: Int))
