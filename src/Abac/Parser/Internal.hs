{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.Internal where

import Data.Char hiding (Space)
import qualified Data.Text as T
import Control.Applicative hiding ((<|>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (Word)

import Abac.Types.ParserTypes
import Abac.Parser.Operations

-- abbreviations

-- String version
withoutAbbreviations' :: String -> String
withoutAbbreviations' = T.unpack . withoutAbbreviations . T.pack

-- Text version
withoutAbbreviations :: Text -> Text
withoutAbbreviations txt = replaceAbbreviations abbrevs (T.toCaseFold txt)

replaceAbbreviations :: [Text] -> Text -> Text
replaceAbbreviations [] txt = txt
replaceAbbreviations (ndl:ndls) txt = replaceAbbreviations ndls (dropdot ndl txt)

dropdot :: Text -> Text -> Text
dropdot abrv txt =
  let abrv' = T.toCaseFold abrv
      abrvNoDot = T.filter (/= '.') abrv'
      txt' = T.toCaseFold txt
  in  T.replace abrv' abrvNoDot txt'

-- abbreviation list including abbreviations prefixed by parens and brackets
abbrevs :: [Text]
abbrevs = abbrevsSpace ++ abbrevsParen ++ abbrevsBrack ++ abbrevsNewln

abbrevs' :: [Text]
abbrevs' = ["i.e.", "viz.", "e.g.", "mr.", "ms.", "mrs.", "prof.", "dr.", "p.", "pp.",
          "Jan.", "Feb.", "Mar.", "Apr.", "Jun.", "Jul.", "Aug.", "Sep.", "sept.", "Oct.",
          "Nov.", "Dec."]

abbrevsParen,abbrevsBrack,abbrevsSpace,abbrevsNewln :: [Text]
abbrevsParen = fmap (T.append "(") abbrevs'
abbrevsBrack = fmap (T.append "[") abbrevs'
abbrevsSpace = fmap (T.append " ") abbrevs'
abbrevsNewln = fmap (`T.append` "\\n") abbrevs'




--Lexeme erases white space characters (' ') before and after a token.

lexeme :: Parser a -> Parser a
lexeme p = wsp >> L.lexeme wsp p where wsp = skipMany $ char ' '

symbl :: String -> Parser String
symbl s = skipMany (char ' ') >> L.symbol space s

dash, ws :: Parser Char
dash = char '-' :: Parser Char
ws = char ' ' :: Parser Char



-- used for links and images

idp :: Parser Text
idp = optional (char ' ') *> openBrckt *> id' <* closeBrckt
  where id' = fmap T.pack $ many $ noneOf [']']

optTitle :: Parser (Maybe String)
optTitle = optional $ lexeme $ qmark *> (many $ noneOf ['"']) <* qmark where qmark = char '"'


openBrckt, closeBrckt, openPrn, closePrn, excl :: Parser Char
openBrckt = char '['
closeBrckt = char ']'
openPrn = char '(' :: Parser Char
closePrn = char ')' :: Parser Char
excl = char '!' :: Parser Char

ws3x, optWs :: Parser (Maybe Char)
ws3x = optWs *> optWs *> optWs :: Parser (Maybe Char)
optWs = optional (char ' ') :: Parser (Maybe Char)


--SecMark Level

sectionMarker :: Parser Marker
sectionMarker = SecMark . length <$> hashes where hashes = lexeme $ some (char '#')

