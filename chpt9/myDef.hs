myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs) =
    if b == True
    then True
    else myOr bs


myAny :: (a -> Bool) -> [a] -> Bool
myAny atoBool (a:as) =
    if atoBool a == True
    then True
    else myAny atoBool as


myElem :: Eq a => a -> [a] -> Bool
myElem a (x:xs) =
    if a == x
    then True
    else myElem a xs


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:as) = myReverse as ++ [a]


squish :: [[a]] -> [a]
squish [] = []
squish (a:as) = a ++ squish as

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap aTob (a:as) = (aTob a) ++ squishMap aTob as


squishAgain :: [[a]] -> [a]
squishAgain a = squishMap (\x -> x) a


myAnyLT :: (a -> Ordering) -> [a] -> Bool
myAnyLT atoBool (a:as) =
    if atoBool a == LT
    then True
    else myAnyLT atoBool as

myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy atoBool (a:as)
    |elem LT (map (atoBool a) as) = myMaximumBy atoBool as
    |otherwise = a





