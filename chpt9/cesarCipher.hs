module Cipher where
import Data.Char


--Implements a caesar cipher on characters only in the a-z range
cipherMapLower :: Int -> Char -> Char
cipherMapLower shift character
  | character `notElem` ['a'..'z'] = toLower character
  | otherwise = chr shiftedCharIndex
      where shiftedCharIndex = unicodeAdjust + 97
            unicodeAdjust = mod (ord (toLower character) - 97 +  shift) 26

cipherMapUpper :: Int -> Char -> Char
cipherMapUpper shift character
  | character `notElem` ['A'..'Z'] = toUpper character
  | otherwise = chr shiftedCharIndex
      where shiftedCharIndex = unicodeAdjust + 65
            unicodeAdjust = mod (ord  character - 65 +  shift) 26

cipherMap :: Int -> Char -> Char
cipherMap shift character
  | isUpper character = cipherMapUpper shift character
  | otherwise         = cipherMapLower shift character

            
caesarCipher :: Int -> String -> String
caesarCipher shift plaintext = map (cipherMap shift) plaintext

uncaesar :: String -> [String]
uncaesar string = map (\x -> caesarCipher x string) [0..25]


uncaesarMap :: String -> [(Int, String)]
uncaesarMap cipherText = zip (reverse [1..26]) $ uncaesar cipherText



