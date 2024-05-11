module AsPatterns where

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _ [] = False 
isSubseqOf check@(c:cs) parent@(p:ps)
  | c == p = isSubseqOf cs ps
  | elem c parent = isSubseqOf check ps
  | otherwise = False 
