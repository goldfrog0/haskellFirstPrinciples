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
            
--combines upper and lower cipher to employ c cipher only on alpha characters.

cipherMap :: Int -> Char -> Char
cipherMap shift character
  | isUpper character = cipherMapUpper shift character
  | otherwise         = cipherMapLower shift character

-- same as above, but uses a char as a key for shift
cipherMapChr :: Char -> Char -> Char
cipherMapChr shift character
  | isUpper character = cipherMapUpper chrshift character
  | otherwise         = cipherMapLower chrshift character
  where chrshift = chrToInt shift 


-- ceasar shifts via a char key wehre the shift is the alphabet position - 1
caesarCipherChr :: Char -> String -> String
caesarCipherChr shift plaintext = map (cipherMap charShift) plaintext
  where charShift = chrToInt shift

--caesar shifts string                  
caesarCipher :: Int -> String -> String
caesarCipher shift plaintext = map (cipherMap shift) plaintext

--creates list to help decode ceaser strings. 
uncaesar :: String -> [String]
uncaesar string = map (\x -> caesarCipher x string) [0..25]

--collects strings in a matched list
uncaesarMap :: String -> [(Int, String)]
uncaesarMap cipherText = zip (reverse [1..26]) $ uncaesar cipherText

--implements a vigenereCipher. ignores non alpha characters
vigenereCipher :: String -> String -> String
vigenereCipher key plaintext = map (\x -> cipherMapChr (snd x) (fst x)) zipped
  where
    zipped = vkeyZip keykey plaintext
    keykey = concat $ replicate (length plaintext) key 
      
vkeyZip :: String -> String ->[(Char, Char)]
vkeyZip [] x = [('_','_')]
vkeyZip _ [] = []
vkeyZip key@(k:ks) str@(x:xs)
  | elem x (['a'..'z'] ++ ['A'..'Z']) = [(x,k)] ++ vkeyZip ks xs
  | otherwise = [(x,x)] ++ vkeyZip key xs


--converts alpha characters to caesar shift equivalent
chrToInt :: Char -> Int
chrToInt char
  | elem char (['a'..'z']++['A'..'Z']) = ord (toLower char) - 97
  | otherwise = 0
