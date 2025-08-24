data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show)

unbalancedTree :: Tree Int
unbalancedTree =
  Node (Leaf 1) (Node (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4)) (Leaf 5))

balancedTree :: Tree Int
balancedTree =
  Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = diff <= 1
  where
    diff = abs ((leaves l) - (leaves r))
