import System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

readLine :: IO String
readLine = do
  s <- doReadLine
  return (cleanup s)

doReadLine :: IO String
doReadLine = do
  c <- getCh
  if c == '\n'
    then return []
    else if c == '\DEL'
           then do
             putChar '\b'
             cs <- doReadLine
             return ('\b' : cs)
           else do
             putChar c
             cs <- doReadLine
             return (c : cs)

cleanup :: String -> String
cleanup s = reverse (applyBackspaces (reverse s) 0)

applyBackspaces :: [Char] -> Int -> [Char]
applyBackspaces [] _ = []
applyBackspaces (c:cs) n
  | c == '\b' = applyBackspaces cs (n + 1)
  | n > 0 = applyBackspaces cs (n - 1)
  | otherwise = c : (applyBackspaces cs n)
