rev1 :: [t] -> [t]
rev1 = foldl (\acc e -> (e:acc)) []

rev2 :: [t] -> [t]
rev2 = foldr (\e acc -> append e acc) []
  where
    append x [] = [x]
    append y (x:xs) = x : append y xs

-- The foldl version is simpler, because the combination function for the last element is called with the yet empty accumulator, so that the list is build up from the right to the left.

-- The foldr version is not only more involved, it is also less efficient, because the elements need to be appended to the accumulator rather than prepended.
