discardFirst :: Eq a => a -> [a] -> [a]
discardFirst _ [] = []
discardFirst x (y:ys)
  | x == y = ys
  | otherwise = y : discardFirst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] [] = True
isChoice xs [] = False
isChoice [] xs = True
isChoice (x:xs) (y:ys) = False -- FIXME
