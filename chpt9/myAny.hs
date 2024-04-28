module MyAny where

myAny :: (a -> Bool) -> [a] -> Bool
myAny f (x:xs)
  | f x = True
  | null xs = f x
  | otherwise = myAny f xs


myElem :: Eq a => a -> [a] -> Bool
myElem element list = any (== element) list 

myReverse :: [a] -> [a]
myReverse (x:xs)
  | null xs = [x]
  | otherwise =  myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish list = foldr (++) [] list

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap func list =
  concat $ map func list 

myMaximumBy ::  (a -> a -> Ordering) -> [a] -> a
myMaximumBy compfunc (x:[]) = x 
myMaximumBy compFunc (x:xs)
  | compFunc x otherXs == GT = x
  | compFunc x otherXs == EQ = x
  | otherwise               = otherXs
  where otherXs = myMaximumBy compFunc xs

myMinimumBy ::  (a -> a -> Ordering) -> [a] -> a
myMinimumBy compfunc (x:[]) = x 
myMinimumBy compFunc (x:xs)
  | compFunc x otherXs == LT = x
  | compFunc x otherXs == EQ = x
  | otherwise               = otherXs
  where otherXs = myMinimumBy compFunc xs

