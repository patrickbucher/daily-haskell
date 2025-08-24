data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

tree :: Tree Int
tree = Node (Node (Leaf 2) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))

-- This definition is more efficient than the original, because depending on
-- the comparison result, only a part of the tree is traversed rather than the
-- entire tree.
-- > occurs 5 tree
-- True
-- > occurs 2 tree
-- True
-- > occurs 1 tree
-- False
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r)
  | cmp == LT = occurs x l
  | cmp == EQ = x == y
  | cmp == GT = occurs x r
  where
    cmp = compare x y
