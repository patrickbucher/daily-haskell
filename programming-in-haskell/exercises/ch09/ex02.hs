discardFirst :: Eq a => a -> [a] -> [a]
discardFirst _ [] = []
discardFirst x (y:ys)
  | x == y = ys
  | otherwise = y : discardFirst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = elem x ys && isChoice xs (discardFirst x ys)
