module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen


myLines :: String -> [String]
myLines poem
    |not $ elem '\n' poem = [poem]
    |otherwise =
        [takeWhile ( /= '\n' ) poem] ++
        myLines (drop 1 (dropWhile ( /= '\n') poem))
