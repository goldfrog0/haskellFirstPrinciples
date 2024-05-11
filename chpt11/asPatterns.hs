module AsPatterns where
import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _  = True
isSubseqOf _ [] = False 
isSubseqOf check@(c:cs) parent@(p:ps)
  | c == p = isSubseqOf cs ps
  | elem c parent = isSubseqOf check ps
  | otherwise = False 

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> (x, capWord x)) . words

capWord :: String -> String
capWord [] = []
capWord (x:xs) = toUpper x:xs

capitalizeParagraph :: String -> String
capitalizeParagraph paragraph = unwords $ aux (words paragraph)
  where
    aux :: [String] -> [String]
    aux [] = []
    aux l@(_:[]) = l
    aux (a:b:rest)
      | last a == '.' = a : capWord b : aux rest
      | otherwise = a : aux (b:rest)
