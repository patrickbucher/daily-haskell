inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n + 1 : inc ns

sqr :: [Int] -> [Int]
sqr [] = []
sqr (n:ns) = n ^ 2 : sqr ns

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar)

data Expr
  = Val Int
  | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n) = pure n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safediv n m

--class Applicative m =>
--      Monad m
--  where
--  return :: a -> m a
--  (>>=) :: m a -> (a -> m b) -> m b
--  return = pure
type State = Int

newtype ST a =
  S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  fmap g st =
    S
      (\s ->
         let (x, s') = app st s
          in (g x, s'))

instance Applicative ST where
  pure x = S (\s -> (x, s))
  stf <*> stx =
    S
      (\s ->
         let (f, s') = app stf s
             (x, s'') = app stx s'
          in (f x, s''))

instance Monad ST where
  st >>= f =
    S
      (\s ->
         let (x, s') = app st s
          in app (f x) s')
