#!/usr/bin/env cabal

module ZipList where

{- cabal:
build-depends:  base
              , QuickCheck
              , checkers
-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f (Nil) = Nil
  fmap f (Cons a (List a)) = Cons a
