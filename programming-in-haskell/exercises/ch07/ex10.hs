altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhn :: [Int] -> Bool
luhn xs = (sum bounded) `mod` 10 == 0
  where
    doubled = altMap id (* 2) (reverse xs)
    bounded =
      map
        (\x ->
           if x > 9
             then x - 9
             else x)
        doubled
