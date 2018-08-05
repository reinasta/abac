module Abac.Parser.Operations where

import Data.Monoid ((<>))
import Control.Applicative hiding ((<|>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Prelude hiding (Word)



type Parsec' e s = ParsecT e s IO
type Parser = Parsec' Void String



--add the element which is the result of parser p to the end of the list returned by the parser ps

infixl 4 <+
(<+) :: Parser [a] -> Parser a -> Parser [a]
ps <+ p = ps <> mklist p

abba :: Parser String
abba = string "abb" <+ char 'a' :: Parser String

abbatest :: IO Bool
abbatest = liftA2 (==) (runParserT abba "" "abba")
  (runParserT (string "abba" :: Parser String) "" "abba")

infixl 4 +>
(+>) :: Parser a -> Parser [a] -> Parser [a]
p +> ps = mklist p <> ps

abba1 :: Parser [Char]
abba1 = char 'a' +> string "bba"

abbatest1 :: IO Bool
abbatest1 = liftA2 (==) (runParserT abba1 "" "abba")
  (runParserT (string "abba" :: Parser String) "" "abba")

mklist :: Parser a -> Parser [a]
mklist = fmap (:[])

