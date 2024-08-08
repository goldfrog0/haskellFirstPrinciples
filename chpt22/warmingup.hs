module Warmup where

import Data.Char
import Control.Applicative (Applicative(liftA2))

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

monadTupled :: [Char] -> ([Char], [Char])
monadTupled = do
  a <- cap
  b <- rev
  return (a, b)
