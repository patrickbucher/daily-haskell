import Data.Char

let2int :: Char -> Int
let2int c
  | isUpper c = ord c - ord 'A'
  | isLower c = ord c - ord 'a' + 26

int2let :: Int -> Char
int2let n 
  | n >= 0 && n < 26 = chr (ord 'A' + n)
  | n >= 26 && n < 52 = chr (ord 'a' + (n - 26))

shift :: Int -> Char -> Char
shift n c
  | isUpper c || isLower c = int2let ((let2int c + n) `mod` 52)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

decode :: Int -> String -> String
decode n = encode (-n)
