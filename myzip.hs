myZip :: (Eq a, Eq b) => [a] -> [b] -> [(a, b)]
myZip (a:as) (b:bs)
    | (as == [] || bs == [])  = [(a,b)]
    | otherwise =
        [(a,b)] ++ myZip as bs

myZipwith ::(Eq a, Eq b) => (a -> b -> c) -> [a] -> [b] -> [c]
myZipwith foo (a:as) (b:bs)
    | (as == [] || bs == [])  = [foo a b]
    | otherwise =
        [foo a b] ++ (myZipwith foo as bs)


mZ :: [a] -> [b] -> [(a, b)]
mZ [] _ = []
mZ _ [] = []
mZ (a:as) (b:bs) = [(a, b)] ++ (mZ as bs)

mZw :: (a -> b -> c) -> [a] -> [b] -> [c]
mZw _ _ [] = []
mZw _ [] _ = []
mZw foo (a:as) (b:bs) = [foo a b] ++ (mZw foo as bs)
