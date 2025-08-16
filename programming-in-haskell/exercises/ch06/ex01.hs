fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- fac (-1) produces an infinite recursion:
-- fac (-2), fac (-3), fac (-4) etc.
fac' :: Int -> Int
fac' 0 = 1
fac' n
  | n > 0 = n * fac' (n - 1)
  | otherwise = 0
