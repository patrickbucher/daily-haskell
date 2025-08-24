data Expr
  = Val Int
  | Add Expr Expr

-- > folde id (+) (Add (Val 3) (Val 2))
-- 5
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = (g (folde f g x) (folde f g y))
