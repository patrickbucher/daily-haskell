data Expr
  = Val Int
  | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = (g (folde f g x) (folde f g y))

-- > eval (Add (Val 3) (Val 2))
-- 5
eval :: Expr -> Int
eval = folde id (+)

-- > size (Add (Val 3) (Val 2))
-- 2
size :: Expr -> Int
size = folde (\_ -> 1) (+)
