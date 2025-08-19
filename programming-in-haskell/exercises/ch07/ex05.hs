-- > (curry' denote) 'a' 17
-- "a17"
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x -> \y -> f (x, y)

denote :: (Char, Int) -> String
denote (c, n) = [c] ++ show n

-- > (uncurry' denote') ('a', 17)
-- "a17"
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

denote' :: Char -> Int -> String
denote' c n = [c] ++ show n
