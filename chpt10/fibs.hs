module Fibonacci where

fibsInfList = 1 : scanl (+) 1 fibsInfList

fibsN x = fibsInfList !! x


--first 20 fibs
fibs20 = take 20 fibsInfList
  
--fibs less than 100
fibsShort = takeWhile (<100) fibsInfList


factorialInf  = scanl (*) 1 [1..]

factorial :: Int -> Int 
factorial num = factorialInf !! num
