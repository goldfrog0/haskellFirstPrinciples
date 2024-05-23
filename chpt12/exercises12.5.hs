module Exercise where

notThe :: String -> Maybe String
notThe str = case elem str ["the", "The"] of
               True -> Nothing
               False -> Just str

replaceThe :: String -> String
replaceThe = unwords . map replaceTheA . words
  where replaceTheA word = case notThe word of
                             Nothing -> "a"
                             Just a  -> a

countTheBeforeVowel :: String -> Int
countTheBeforeVowel str = aux 0 $ words str
  where
    aux :: Int -> [String] -> Int
    aux acc []     = acc
    aux acc (_:[]) = acc
    aux acc (a:b:rest)
      | elem a ["the", "The"] && elem (head b) "aeiouAEIOU" =
          aux (acc + 1) rest
      | otherwise = aux acc (b:rest)

countVowels :: String -> Int
countVowels [] = 0
countVowels (x:xs)
  | elem x vowels = 1 + countVowels xs
  | otherwise     = countVowels xs
  where vowels = "aeiouAeiou"

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord word
  | vowels > cons = Nothing
  | otherwise     = Just $ Word' word
  where
    vowels = countVowels word
    cons = length word - vowels

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInt :: Nat -> Int
natToInt Zero     = 0
natToInt (Succ a) = 1 + natToInt a

intToNat :: Int -> Maybe Nat
intToNat num
  | num < 0 = Nothing
  | otherwise = Just $ aux num
  where
    aux 0 = Zero
    aux num =  Succ (aux (num - 1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing   = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee def _ Nothing  = def
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback Nothing = fallback
fromMaybe _       (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just a:xs)  = a:catMaybes xs

flipMaybe :: Eq a =>  [Maybe a] -> Maybe [a]
flipMaybe list
  | elem Nothing  list = Nothing
  | otherwise = Just $ map (\(Just x) -> x) list

--lefts' :: [Either a b] -> [a]
--lefts' [] = []
--lefts' (Left a :rest) = [a] ++ lefts' rest
--lefts' (_:rest)       = lefts' rest

lefts' :: [Either a b] -> [a]
lefts' = foldr takeLefts []
  where
    takeLefts (Left a)  list = a:list
    takeLefts (Right _) list =   list

--rights' :: [Either a b] -> [b]
--rights' [] = []
--rights' (Right b :rest) = [b] ++ rights' rest
--rights' (_:rest)        = rights' rest

rights' :: [Either a b] -> [b]
rights' = foldr takeRights []
  where
    takeRights (Right b) list = b:list
    takeRights (Left _)  list = list

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers list = (lefts' list, rights' list)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ (Left _)  = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa _  (Left a)  = fa a
either' _  fb (Right b) = fb b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f eith = either' (\_ -> Nothing) (\x -> Just $ f x) eith
