module Exercises10 where

--bug fix

bug1 = foldr (++) []  ["woot", "Woot", "woot"]
bug2 = foldr max 'a' "fear is the little death"
bug3 = foldr (&&) True [False, True]
bug4 = foldr (||) False [False, True]
bug5 = foldr ((++) . show) "" [1..5]
bug6 = foldl const 'a' [1..5]
bug7 = foldl const 0 "tacos"
bug8 = foldl const 0 "burritos"
bug9 = foldl const 'z' [1..5]
