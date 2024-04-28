eftBool :: Bool -> Bool -> [Bool]
eftBool True True   = [True]
eftBool True False  = []
eftBool False True  = [False, True]
eftBool False False = [False]


eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd = undefined


eftInt :: Int -> Int -> [Int]
eftInt x0 xn
    |x0 > xn   = []
    |x0 == xn  = [x0]
    |otherwise =
        [x0] ++ (eftInt (x0 + 1) xn)

eftChar :: Char -> Char -> [Char]
eftChar c0 cn
    |c0 > cn  = []
    |c0 == cn = [c0]
    |otherwise =
        [c0] ++ (eftChar (succ c0) cn)

