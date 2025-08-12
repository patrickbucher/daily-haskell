luhnDouble :: Int -> Int
luhnDouble x =
  if y > 9
    then y - 9
    else y
  where
    y = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = total `mod` 10 == 0
  where
    total = sum [luhnDouble a, b, luhnDouble c, d]
