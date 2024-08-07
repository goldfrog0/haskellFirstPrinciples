module TravPrac where

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> x  = fmap f x

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance Monoid a =>  Monoid (Identity a) where
  mempty = mempty

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x


newtype Constant a b = Constant { getConstant :: a }
  deriving (Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  -- traverse :: (Applicative f) => (a -> f b) -> Constant a -> f (Constant b)
  traverse f (Constant x) = liftA Constant (pure x)


--instance Traversable Identity where
--  traverse d (Identity a) =  undefined
