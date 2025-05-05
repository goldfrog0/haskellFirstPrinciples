module Parsing where

import Text.Trifecta (Parser, unexpected, char, parseString, choice,
                                string)
import Text.Parser.Combinators (eof)
import Control.Applicative
import Control.Monad.RWS (Monoid(mempty))


stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  Prelude.putStrLn ('\n' : s)

oneEof = one >> eof

exerOne = do
  pNL "oneEof success:"
  print $ parseString oneEof mempty "1"
  pNL "oneEof fail:"
  print $ parseString oneEof mempty "12"

multString :: Parser ()
multString = (string "123" <|> string "12" <|> string "1") >> eof

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
