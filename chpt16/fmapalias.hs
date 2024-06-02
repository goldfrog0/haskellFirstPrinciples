module Practice where

import Test.QuickCheck

data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe   = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)


data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) =
    Heisenberg (n) (f a)

replaceWithP :: a -> Char
replaceWithP = const 'p'

a :: [Int]
a = (+1) <$> read "[1]" :: [Int]

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c :: Int -> Int
c =  (*2) . (\x -> x - 2)

d :: Integer -> [Char]
d =
  ((return '1' ++) . show).
  (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = (read . ("123"++) . show) <$> ioi
    in fmap (*3) changed

data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f  == f

functorCompose :: (Eq (f c), Functor f) =>
                        (a -> b)
                     -> (b -> c)
                     -> f a
                     -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

--instance Functor (Two b) where
  --fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c

instance Functor (Three b c) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' b) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

data Four a b c d = Four a b c d

instance Functor (Four b c d) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b

instance Functor (Four' b) where
  fmap f (Four' a b c b') = Four' a b c (f b')

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

data Wrap f a =
  Wrap (f a)
  deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

getInt :: IO Int
getInt = fmap read getLine

meowthIsm :: IO String
meowthIsm = do
  input <- getLine
  return (input ++ "! Meowth thats right!")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)
