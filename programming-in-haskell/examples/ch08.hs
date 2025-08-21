safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safetail :: [a] -> Maybe [a]
safetail [] = Nothing
safetail xs = Just (tail xs)

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

data List a
  = Nil
  | Cons a (List a)

listLength :: List a -> Int
listLength Nil = 0
listLength (Cons _ x) = 1 + (listLength x)
