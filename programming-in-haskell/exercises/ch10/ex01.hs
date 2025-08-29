putStr :: String -> IO ()
putStr cs = sequence_ [putCharc | c <- cs]
