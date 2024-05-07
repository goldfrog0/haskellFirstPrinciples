module KindsPrac where

data PugType = PugData

--PugType is the typeconstructor. no args taken, like a constant type
-- functions requiring a value of PugType, only PugData is a valid input.

data HuskyType a = HuskyData

-- HuskyType is the type constructor, taking only a single type variable.
--notice that a is not present after =. a is  phantom, 

data DogueDeBordeaux doge =
  DogueDeBordeaux doge

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

-------------------------EXERCISES

data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)


data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
  | Plane Airline Size 
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 1234)

  
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane (Plane _  _) = True
isPlane _           = False 

areCars :: [Vehicle] -> [Bool]
areCars = map isCar


getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man



class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Cows =
  Cows Int deriving (Eq, Show)

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43
