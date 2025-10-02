describe :: Show a => [a] -> String
describe all@(x:_) =
  "list of length " ++ show (length all) ++ " with head " ++ show x

judgeBMI :: Double -> Double -> String
judgeBMI weight height
  | bmi < 20 = "underweight"
  | bmi < 25 = "normal weight"
  | bmi < 30 = "overweight"
  | otherwise = "obese"
  where
    bmi = weight / height ^ 2

pythagoras :: Double -> Double -> String
pythagoras a b =
  let c = sqrt (a ^ 2 + b ^ 2)
   in show a ++ "^2+" ++ show b ++ "^2=" ++ show c ++ "^2"

triplets :: Int -> [(Int, Int, Int)]
triplets n =
  [ (a, b, c)
  | a <- [1 .. n]
  , b <- [a .. n]
  , c <- [b .. n]
  , let a' = a ^ 2
  , let b' = b ^ 2
  , let c' = c ^ 2
  , a' + b' == c'
  ]
