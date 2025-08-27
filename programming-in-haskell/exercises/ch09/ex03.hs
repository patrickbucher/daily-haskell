data Op
  = Add
  | Sub
  | Mul
  | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expr
  = Val Int
  | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

split' :: [a] -> [([a], [a])]
split' [] = [([],[])]
split' [x] = [([x],[])]
split' (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split' xs]

exprs' :: [Int] -> [Expr]
exprs' [] = []
exprs' [n] = [Val n]
exprs' ns =
  [e | (ls, rs) <- split' ns, l <- exprs' ls, r <- exprs' rs, e <- combine l r]

-- exprs' [1,2]
-- [e | (ls, rs) <- [([1],[2]),([1,2],[])], l <- exprs' ls, r <- exprs' rs, e <- combine l r]
-- [e | (ls, rs) <- [([1,2],[])], l <- exprs' ls, r <- exprs' rs, e <- combine l r]
-- exprs' [1,2] would process the pair ([1,2],[]), which would then call exprs' [1,2] recursively.
-- The recursion would not terminate. (Stack Overflow Error)

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]
