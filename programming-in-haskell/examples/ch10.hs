getLine :: IO String
getLine = do
  c <- getChar
  if c == '\n'
    then return []
    else do
      cs <- Main.getLine
      return (c : cs)

putString :: String -> IO ()
putString [] = return ()
putString (c:cs) = do
  putChar c
  putString cs

putLine :: String -> IO ()
putLine cs = do
  putString cs
  putChar '\n'
