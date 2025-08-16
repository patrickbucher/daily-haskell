euclid :: Int -> Int -> Int
euclid 1 1 = 1
euclid a b
  | a == b = a
  | a > b = euclid (a - b) b
  | a < b = euclid a (b - a)
