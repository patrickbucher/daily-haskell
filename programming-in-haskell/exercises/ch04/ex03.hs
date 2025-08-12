safetail :: [a] -> [a]
safetail xs =
  if null xs
    then []
    else tail xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs
