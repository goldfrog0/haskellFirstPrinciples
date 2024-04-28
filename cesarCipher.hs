module Cipher where
import Data.Char


--Implements a caesar cipher on characters only in the a-z range
cipherMap :: Int -> Char -> Char
cipherMap shift character
  | (toLower character) `notElem` ['a'..'z'] = toLower character
  | otherwise = chr shiftedCharIndex
      where shiftedCharIndex = unicodeAdjust + 97
            unicodeAdjust = mod (ord (toLower character) - 96 +  shift) 26
caesarCipher :: Int -> String -> String
caesarCipher shift plaintext = map (cipherMap shift) plaintext

uncaesar :: String -> [String]
uncaesar string = map (\x -> caesarCipher x string) [0..25]

uncaesarMap :: String -> [(Int, String)]
uncaesarMap cipherText = zip (reverse [1..26]) $ uncaesar cipherText


