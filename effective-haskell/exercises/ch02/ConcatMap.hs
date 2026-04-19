concatMap1 f = foldl (\acc -> \e -> acc <> f e) []
concatMap2 f = foldr (\e -> \acc -> f e <> acc) []

-- The foldl version tends to append smaller lists to the end of larger ones, which is potentially less efficient than the foldr version.
