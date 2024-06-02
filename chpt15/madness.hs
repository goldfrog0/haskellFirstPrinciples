module Madness where

import Test.QuickCheck
import Data.Monoid
import Data.Semigroup

type Verb       = String
type Adjective  = String
type Adverb     = String
type Noun       = String
type Exclamation = String

madLibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madLibbin' e adv noun adj =
  e    <> "! he said " <>
  adv  <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj  <> " wife."
-- write using mconcat
madLibbin :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madLibbin e adv noun adj =
  mconcat [e,"! he said "
          , adv, " as he jumped into his car "
          , noun, " and drove off with his "
          , adj, " wife."]

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semiGroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semiGroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc =
  Identity String -> Identity String -> Identity String -> Bool

--------------------------------
data Two a b = Two a b deriving(Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc =
  Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = fmap Combine arbitrary

newtype Comp a =
  Comp (a -> a)

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp id

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
      }

instance Semigroup a => Semigroup (Mem s a) where
 (Mem f) <> (Mem g) =
   Mem $ \s ->
     let (a', s') = g s
         (a'', s'') = f s'
     in  (a'' <> a', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s (mempty, s)

main :: IO()
main = do
  quickCheck (semiGroupAssoc :: TrivAssoc)
  quickCheck (semiGroupAssoc :: IdentityAssoc)

  quickCheck (semiGroupAssoc :: TwoAssoc)
