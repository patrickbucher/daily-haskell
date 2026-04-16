module Main where

uncurriedAddition ab = (fst ab) + (snd ab)

curriedAddition a b = a + b

curry' f = \a -> \b -> f (a, b)

uncurry' f = \(a, b) -> f a b

main =
  print $ addOne 100
  print $ add' (2, 3)
  where
    add = curry' uncurriedAddition
    addOne = add 1
    add' = uncurry' curriedAddition

