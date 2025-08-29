type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard b = sequence_ [putRow r n | (r, n) <- zip [1 ..] b]

doPutBoard :: Board -> Int -> IO ()
doPutBoard _ 0 = return ()
doPutBoard b r = do
  putRow r (b !! (r - 1))
  doPutBoard b (r - 1)
