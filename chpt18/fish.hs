module Prac  where

import Control.Monad

sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you?"

data Nope a =
  NopeDotJpg

instance Functor (Nope) where
  fmap _ _ = NopeDotJpg

instance Applicative (Nope) where
  pure _ = NopeDotJpg

  (<*>) _ _ = NopeDotJpg

instance Monad (Nope) where
  return = pure
  _ >>= _ = NopeDotJpg
--------------------------------------------
data PhbtEither a b =
    Left' a
  | Right' b

instance Functor (PhbtEither b) where
  fmap _ (Left' x)  = Left' x
  fmap f (Right' x) = Right' (f x)

instance Applicative (PhbtEither b) where
  pure x = Right' x
  Left' x <*> _ = Left' x
  _ <*> Left' x = Left' x
  Right' f <*> Right' x = Right' (f x)

instance Monad (PhbtEither b) where
  return b = Right' b
  (Left' a) >>= _ = Left' a
  (Right' a) >>= f = f a

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> l = l
  Cons a rest <> l = Cons a (rest <> l)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> ls = fmap f (ls) <> (fs <*> ls)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = f x <> (xs >>= f)

j :: Monad m => m (m a) -> m a
j x =  x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = fmap f a

a' :: Monad m => m a -> m (a -> b) -> m b
a' x mf = mf <*> x

meh :: Monad m
  => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = f x >>=
               \y -> meh xs f >>=
               \ys -> return $ y : ys

flipType :: Monad m => [m a] -> m [a]
flipType mlist =
  meh mlist id
