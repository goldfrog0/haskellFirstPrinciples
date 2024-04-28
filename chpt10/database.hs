module DataBase where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDataBase :: [DatabaseItem]
theDataBase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getDates []
  where
    getDates (DbDate date) rest = date : rest
    getDates _             rest = rest

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNums []
  where
    getNums (DbNumber num) rest = num : rest
    getNums _             rest = rest

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate 

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb ::  [DatabaseItem] -> Double
avgDb dataBase = numerator / denom
  where
    numerator = (fromIntegral $ sumDb dataBase)
    denom = fromIntegral (length $ filterDbNumber dataBase)
