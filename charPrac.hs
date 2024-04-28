import Data.Char


onlyUpper :: String -> String
onlyUpper str = filter isUpper str


capFst :: String -> String
capFst (s:entence) = (toUpper s :entence)


cappy :: String -> String
cappy [] = []
cappy (x:xs) = [toUpper x] ++ (cappy xs)


takeFstCap :: String -> Char
takeFstCap str =  toUpper $ head str


takeFstCapC :: String -> Char
takeFstCapC str =  (toUpper.head) str

takeFstCapCPF :: String -> Char
takeFstCapCPF = toUpper.head

