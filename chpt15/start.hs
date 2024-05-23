module Prac where

--instance Monoid b => Monoid (a -> b)
--
--instance (Monoid a, Monoid b) => Monoid (a, b)

--instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)

data Booly a =
    True'
  | False'
  deriving (Eq, Show)

instance Monoid (Booly a) where
  mempty = True'
instance Semigroup (Booly a) where
   False' <>  _     = False'
   _      <> False' = False'
   True'  <> True'  = True'

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
        => Monoid (Optional a) where
  mempty = Nada

--now requires a semigroup instance
-- this is where you define the mappend operation.
instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada
