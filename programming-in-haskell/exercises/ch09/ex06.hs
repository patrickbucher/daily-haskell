import Data.List

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x > y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = x > 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr
  = Val Int
  | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x :) yss
  where
    yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
  | (ls, rs) <- split ns
  , lx <- results ls
  , ry <- results rs
  , res <- combine' lx ry
  ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  map snd
    $ sortBy
        (\l -> \r -> compare (fst l) (fst r))
        [(complexity e, e) | ns' <- choices ns, (e, m) <- results ns', m == n]

approximateSolutions :: [Int] -> Int -> [Expr]
approximateSolutions ns n = take 10 $ map snd candidates
  where
    candidates =
      sortBy
        (\l -> \r -> compare (fst l) (fst r))
        [((abs (m - n), e)) | ns' <- choices ns, (e, m) <- results ns']

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n =
  if length exactSolutions > 0
    then exactSolutions
    else approximateSolutions ns n
  where
    exactSolutions = solutions' ns n

complexity :: Expr -> Int
complexity (Val n) = 1
complexity (App o l r) = cmpx o + complexity l + complexity r
  where
    cmpx Add = 1
    cmpx Sub = 2
    cmpx Mul = 3
    cmpx Div = 4
    cmpx Exp = 5

main :: IO ()
main = print (solutions' [1, 3, 7, 10, 25, 50] 765)
-- 6b)
-- main = print (solutions'' [1, 3, 7, 10, 25, 50] 831)
-- 10*(((50+1)+7)+25) = 830
-- 10*((50+(7+1))+25) = 830
-- ...
