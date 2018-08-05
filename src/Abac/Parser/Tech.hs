module Abac.Parser.Tech where


import Data.Monoid ((<>))
import Control.Applicative hiding ((<|>))
import Data.Char hiding (Space)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (Word)


import Abac.Types.ParserTypes
import Abac.Internal (posFromSource)
import Abac.Parser.Operations
import Abac.Parser.Internal



--tech

techInline :: Parser Inline
techInline = try mathInline <|> codeInline

codeInline :: Parser Inline -- Inline Code Text
codeInline = do
  codeStr <- between (char '`') (char '`') (many $ noneOf ['`'])
  pos <- posFromSource (length codeStr) <$> getPosition
  return $ InlineTech Code pos (T.pack codeStr)
  <?> "inline code"


mathInline :: Parser Inline
mathInline = do
  mathstr <- try dollarMath <|> parensMath
  pos <- posFromSource (length mathstr) <$> getPosition
  return $ InlineTech Math pos (T.pack mathstr)
  <?> "inline math"
  where
    dollarMath = mathString dollar dollar
    parensMath = mathString openParen closeParen


mathString :: Parser String -> Parser String -> Parser String
mathString opn cls = lexeme opn *> mathtail cls
  where
    mathtail, nonend, end, till :: Parser String -> Parser String
    mathtail str = concat <$> many (try (nonend str) <|> try (end str))
    nonend str = try (till mbox) <|> till (esc <> str)
    end str = manyTill anyChar str
    till str = manyTill anyChar (lookAhead str) <> str
    mbox :: Parser String
    mbox = mathTextMacroMarker <> symbl "{" <> (many $ noneOf ['}']) <> symbl "}"


--open and closing math markers plus escape string

dollar, openParen, closeParen, ddollar, openBracket, closeBracket, esc :: Parser String
dollar = string "$" :: Parser String
openParen = string "\\(" :: Parser String
closeParen = string "\\)" :: Parser String
ddollar = string "$$" :: Parser String
openBracket = string "\\[" :: Parser String
closeBracket = string "\\]" :: Parser String
esc = string "\\" :: Parser String

openMathBlock :: Parser String
openMathBlock = try (string "\\[") <|> string "\\begin{equation}" :: Parser String

closeMathBlock :: Parser String
closeMathBlock = try (string "\\]") <|> string "\\end{equation}" :: Parser String

mathTextMacroMarker :: Parser String
mathTextMacroMarker =
  choice [ string "\\mbox"
         , string "\\text" <* (try (letterChar >> letterChar) :: Parser Char)
         ]

