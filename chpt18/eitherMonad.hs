module EitherMonad where

import Control.Monad

type Founded = Int

type Coders = Int

data SoftwareShop =
  Shop {
         founded     :: Founded
       , programmers :: Coders
       } deriving (Eq, Show)

data FoundedError =
      NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0    = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded' <- validateFounded years
  programmers' <- validateCoders coders
  if programmers' > div founded' 10
    then Left $
           TooManyCodersForYears
           founded' programmers'
    else Right $ Shop founded' programmers'

--define either monad
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum b) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure a = Second a
  (<*>) _ (First a) = First a
  (<*>) (First a) _ = First a
  (<*>) (Second f) (Second b) = Second $ f b

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second a) f = f a
