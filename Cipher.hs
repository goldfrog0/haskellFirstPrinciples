module Cipher where

import Data.Char


-- shifts only capital and lowercase alpha chars by a value
-- "shift".
ceaserShift :: Int -> [Int] -> [Int]
ceaserShift _ [] = []
ceaserShift shft (x:xs)
--shifts alphabet by the shift, does not touch anything else
    |(x > 96) && (x < 123) = [(mod (x - 97 +shft) 26) + 97] ++
        ceaserShift shft xs
    |(x > 64) && (x < 91)  = [(mod (x - 64 +shft) 26) + 64] ++
        ceaserShift shft xs
    |otherwise = [x] ++ ceaserShift shft xs


-- need func that converts string -> [Int]
strLint :: String -> [Int]
strLint = (map ord)

-- need func that converts [int] -> String
lintStr :: [Int] -> String
lintStr = (map chr)

ceaserEnc :: Int -> String -> String
ceaserEnc shift str =
    lintStr (ceaserShift shift (strLint str))


--Function that unshifts a traditional ceaser ciper
ceaserDec :: Int -> String -> String
ceaserDec  unshift encMsg = ceaserEnc (-unshift) encMsg
