module TreeMap where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a
        => a
        -> BinaryTree a
        -> BinaryTree a

insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


treeMap :: (a -> a) -> BinaryTree a -> BinaryTree a
treeMap f Leaf = Leaf
treeMap f (Node leftB a rightB) = (Node (treeMap f leftB) (f a) (treeMap f rightB))

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
        1
        (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay =
  if treeMap (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"
