module Main (main) where

import Lib
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

--instance Monoid Bull where
--  mempty = Fools
--  mappend _ _ = Fools
--
instance EqProp Bull where (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  x <> Nil = x
  Nil <> x = x
  (Cons a as) <> xs = Cons a (as <> xs)

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil

  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs  <> (fs <*> xs)

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)




main :: IO ()
main = undefined --quickBatch (monoid Twoo)
