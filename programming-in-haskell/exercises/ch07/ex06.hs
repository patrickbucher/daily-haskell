unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

chop8' :: [Int] -> [[Int]]
chop8' = unfold (== []) (take 8) (drop 8)

map' :: Eq a => (a -> b) -> [a] -> [b]
map' f = unfold (== []) (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f
