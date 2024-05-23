module UnfoldPractice where

myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f m = case f m of Nothing -> []
                            Just (a,b) -> a:myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just(x, f  x)) x

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of Nothing -> Leaf
                         Just (l, m, r) -> Node (unfold f l) m (unfold f r)


treeBuild :: Integer -> BinaryTree Integer
treeBuild int = unfold builder int
  where
    builder int | int <= 0  = Nothing
                | otherwise = Just (int -1, int -1, int -1)
