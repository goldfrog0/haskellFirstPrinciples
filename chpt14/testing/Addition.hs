-- | 

module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise =
           go (n-d) d (count + 1)

recursiveMult :: (Eq a, Integral a, Ord a) => a -> a -> a
recursiveMult m n
  | m == 0    = 0
  | m < 0     = recursiveMult (-m) (-n)
  | otherwise = n + recursiveMult (m - 1) n

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
         => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c)
            => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b)
        => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

--equal chance of nothing and just values
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO()
runQc = quickCheck prop_additionGreater

main :: IO()
main = hspec $ do
  describe "Addition" $ do
    it "-3 * 8 = -24" $ do
      recursiveMult (-3) 8 `shouldBe` (-24)
    it "-9 * -20 = 180" $ do
      recursiveMult (-9) (-20) `shouldBe` 180
    it "3 * 3 = 9" $ do
      recursiveMult 3 3 `shouldBe` 9
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x::Int)
