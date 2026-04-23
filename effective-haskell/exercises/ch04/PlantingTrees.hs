data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

showStringTree :: BinaryTree String -> String
showStringTree Leaf = ""
showStringTree (Branch l m r) = showStringTree l <> m <> showStringTree r

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf x = (Branch Leaf x Leaf)
addElementToIntTree (Branch l m r) x =
  if x <= m
  then (Branch (addElementToIntTree l x) m r)
  else (Branch l m (addElementToIntTree r x))

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf x = False
doesIntExist (Branch l m r) x =
  m == x || doesIntExist l x || doesIntExist r x
