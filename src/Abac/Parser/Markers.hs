module Abac.Parser.Markers where

import Data.Char hiding (Space)
import qualified Data.Text as T
import Prelude hiding (Word)

import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug (dbg)

import Abac.Types.ParserTypes
import Abac.Parser.Operations




--parse markers

inlineMarker :: Parser Inline
inlineMarker = fmap Other marker

submrk :: Parser Marker
submrk = do
  try (dbg "newline" newline) <|> return undefined
  mrk <- dbg "it/exmrk" (try itmrk <|> exmrk)
  check mrk
  where
    check :: Marker -> Parser Marker
    check mrk' = dbg "check" (
      if markerLevel mrk' < 1
         then fail "level 0 marker"
         else return mrk')

markerNoNewline :: Parser Marker
markerNoNewline = try itmrk <|> exmrk <?> "marker without a newline-prefix"

marker :: Parser Marker
marker = newline *> (try itmrk <|> exmrk <?> "marker")

exmrk :: Parser Marker
exmrk = do
  sps <- many (char ' ')
  (int, nom) <- try exMarker1 <|> exMarker2
  return $ ExMark (length sps `div` 2) int nom
  where
    exMarker2 :: Parser (No, Name)
    exMarker2 = do
      char '('
      char '@'
      nom <- many $ noneOf (")" :: String)
      char ')'
      skipMany (char ' ')
      return (zeros, T.pack nom)

    exMarker1 = exMarker11 <|> exMarker12

    exMarker11 :: Parser (No, Name)
    exMarker11 = do
      int <- punctAround numkey
      skipMany (char ' ')
      let nom = T.pack $ show int
      let no = (int,0,0,0,0,0)
      return (no, nom)

    exMarker12 :: Parser (No, Name)
    exMarker12 = do
      nom <- punctAround alphakey
      skipMany (char ' ')
      return (zeros, T.pack nom)

    numkey = read <$> some (digitChar :: Parser Char) :: Parser Int
    alphakey = fmap pure letterChar <|> many (oneOf romanDigits) -- some (letterChar :: Parser Char)
    romanDigits = "ivxl" :: String
    punctAround :: Parser a -> Parser a
    punctAround content =
      try (char '(' *> content <* char ')')
      <|> content <* (char '.' <|> char ')')




itmrk :: Parser Marker
itmrk = do
  sps <- many (char ' ')
  notFollowedBy emphMarkerLeft
  chr' <- char '+' <|> char '-' <|> char '*'
  skipMany (char ' ')
  return $ ItMark (length sps `div` 2) zeros (T.singleton chr')

--markers' levels and numbers

exLevel :: Marker -> Level
exLevel (ExMark lev _ _) = lev
exLevel _ = 0

itLevel :: Marker -> Level
itLevel (ItMark lev _ _) = lev
itLevel _ = 0

markerLevel :: Marker -> Level
markerLevel mrk@(ExMark _ _ _) = exLevel mrk
markerLevel mrk@(ItMark _ _ _) = itLevel mrk
markerLevel _ = 0

updateExNo :: No -> Marker -> Marker
updateExNo no (ExMark lev _ nom) = ExMark lev no nom
updateExNo _ mrk = mrk

updateItNo :: No -> Marker -> Marker
updateItNo no (ItMark lev _ nom) = ItMark lev no nom
updateItNo _ mrk = mrk

updateMarkerNo :: No -> Marker -> Marker
updateMarkerNo no (ExMark lev _ nom) = ExMark lev no nom
updateMarkerNo no (ItMark lev _ nom) = ItMark lev no nom
updateMarkerNo no (FtnMark _ nom) = FtnMark no nom
updateMarkerNo no (RefEx _ nom) = RefEx no nom
updateMarkerNo no (RefFtn _ nom) = RefFtn no nom
updateMarkerNo _ m = m


-- emphasis markers
emphMarkerLeft, emphMarkerRight :: Parser String
emphMarkerLeft = string "*" <* notFollowedBy (char '*') <* lookAhead alphaNumChar
emphMarkerRight = string "*" <* notFollowedBy (char '*') <*
  lookAhead (spaceChar $> () <|> punctuationChar $> () <|> eof)

boldMarkerLeft, boldMarkerRight :: Parser String
boldMarkerLeft = string "**" <* lookAhead alphaNumChar
boldMarkerRight = string "**" <* lookAhead (spaceChar $> () <|> punctuationChar $> () <|> eof)

-- balancing punctuation
leftParen, rightParen, leftBracket, rightBracket, leftQuote, rightQuote :: Parser String
leftParen = string "(" :: Parser String
rightParen = string ")" :: Parser String
leftBracket = string "[" :: Parser String
rightBracket = string "]" :: Parser String
leftQuote = string "\'" <|> string "\8216" <* lookAhead (alphaNumChar <|> punctuationChar) :: Parser String
rightQuote = string "\'" <|> string "\8217" <* lookAhead (spaceChar $> () <|> punctuationChar $> () <|> eof) :: Parser String

leftDoubleQuote, rightDoubleQuote :: Parser String
leftDoubleQuote = string "\"" <|> string "\8220" <* lookAhead (alphaNumChar <|> punctuationChar)
rightDoubleQuote = string "\""  <|> string "\8221" <* (lookAhead (spc <|> pct <|> eof))
  where
    spc = spaceChar $> ()
    pct = punctuationChar $> ()

