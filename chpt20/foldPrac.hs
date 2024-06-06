module Prac where

import Data.Monoid

data Identity a =
  Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x ) = f z x
  foldMap f (Identity x) = f x

data Optional a = Nada | Yep a

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a


mySum :: (Foldable t, Num a) => t a -> a
mySum = getSum . foldMap Sum

myProduct :: (Foldable t, Num a) => t a -> a
myProduct = getProduct . foldMap Product

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem x xs = getAny . foldMap (Any . (== x)) $ xs

myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
myMaximum fl = foldr maxs Nothing fl
  where
    maxs x Nothing = Just x
    maxs x (Just y) = Just (max x y)

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum fl = foldr mins Nothing fl
  where
    mins x Nothing = Just x
    mins x (Just y) = Just (min x y)

myNull :: Foldable t => t a -> Bool
myNull = foldr (\_ _ -> False) True

myLength :: (Foldable t) => t a -> Int
myLength = foldr (\_ acc -> acc + 1) 0

myToList :: (Foldable t) => t a -> [a]
myToList = foldr (\x acc -> x : acc) []

myFold :: (Foldable t, Monoid m) => t m -> m
myFold = foldMap (id)

--------------------

data Constant a b =
  Constant b
  deriving Show

instance Foldable (Constant a) where
  foldMap _ _ = mempty

--------------------

data Two a b = Two a b deriving Show


instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

--------------------

data Three a b c = Three a b c deriving Show

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

--------------

data Three' a b = Three' a b b deriving Show

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = (f b) <> (f b')

-----------------

data Four' a b =
  Four' a b b b
  deriving Show

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = (f b) <> (f b') <> (f b'')


filterF :: (Applicative f
           , Foldable t
           , Monoid (f a))
           => (a -> Bool) ->  t a -> f a
filterF f l = foldMap (\x -> if f x then pure x else mempty) l
