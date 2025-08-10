hypot :: (Float, Float) -> Float
hypot (a, b) = sqrt (a ^ 2 + b ^ 2)

hypot' :: Float -> (Float -> Float)
hypot' a b = sqrt (a ^ 2 + b ^ 2)
