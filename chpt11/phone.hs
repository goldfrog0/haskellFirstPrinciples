module Phone where


type Key = Char
data Keypad = Keypad [(Key, [Char])] deriving Show

phoneDigits :: Keypad
phoneDigits = Keypad [('1', "1"),
                      ('2', "abc"),
                      ('3', "def"),
                      ('4', "ghi"),
                      ('5', "jkl"),
                      ('6', "mno"),
                      ('7', "pqrs"),
                      ('8', "tuv"),
                      ('9', "wxyz"),
                      ('*', "^*"),
                      ('0', "+ 0"),
                      ('#', ".,#")]
type Digit = Char
type Presses = Int

--keysToStr :: [(Digit, Presses)] -> String
