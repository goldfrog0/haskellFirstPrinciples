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
  arbitrary = return Identity (arbitrary a)

type IdentAssoc =

main :: IO()
main =
  quickCheck (semiGroupAssoc :: TrivAssoc)
