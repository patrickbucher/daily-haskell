p :: Num a => (a -> a) -> (a -> a -> a) -> [a] -> a
p = \f g -> foldr g 0 . map f

q :: Num a => (a -> a) -> (a -> a -> a) -> [a] -> a
q = \f g -> foldr (g . f) 0

-- p (*2) (+) [1,2,3]
-- 12

-- q (*2) (+) [1,2,3]
-- 12

p' :: Num a => (a -> a) -> (a -> a -> a) -> [a] -> a
p' = \f g -> foldl g 0 . map f

q' :: Num a => (a -> a) -> (a -> a -> a) -> [a] -> a
q' = \f g -> foldl (g . f) 0

-- p' (*2) (+) [1,2,3]
-- 12

-- q' (*2) (+) [1,2,3]
-- 11

-- The composition of q' (+ . (2*)) is first applied to the initial accumulator of 0, with which the first element of the list is multiplied.
-- The composition of q (+ . (2*)) is first applied to the initial element of the list, to which the initial accumulator of 0 is added.
