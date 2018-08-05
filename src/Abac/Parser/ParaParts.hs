{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.ParaParts where


import Data.Monoid ((<>))
import Text.Megaparsec
import Text.Megaparsec.Char


import Abac.Types.ParserTypes
import Abac.Parser.Operations
import Abac.Parser.Inlines
import Abac.Parser.Markers


-- most general parser of paragraph parts
decoratedParaParts :: Parser [ParaPart]
decoratedParaParts = fmap concat $
  many $
    try bracks'
    <|> try parens'
    <|> try doubles'
    <|> try quotes'
    <|> try emphs'
    <|> try bolds'
    <|> manyParaParts

decoratedParaPartsNonempty :: Parser [ParaPart]
decoratedParaPartsNonempty = do
  prts <- decoratedParaParts
  if null prts then fail "empty para-parts" else return prts

decoratedParaPartsWithTest :: Parser [ParaPart]
decoratedParaPartsWithTest = try decoratedParaPartsNonempty <|> decoratedParaParts




-- parser for balancing punctuation

bracks' :: Parser [ParaPart]
bracks' =
  withinBalancing Bracketed
    (leftBracket)
    (rightBracket)
    (contentsWithinBracksWith' manyParaParts)

parens' :: Parser [ParaPart]
parens' =
  withinBalancing Parenthetical
    leftParen
    rightParen
    (contentsWithinParensWith' manyParaParts)

doubles' :: Parser [ParaPart]
doubles' =
  withinBalancing DoubleQuoted
    (leftDoubleQuote)
    (rightDoubleQuote)
    (contentsWithinDoublesWith' manyParaParts)

quotes' :: Parser [ParaPart]
quotes' =
  withinBalancing Quoted
    leftQuote
    rightQuote
    (contentsWithinQuotesWith' manyParaParts)

emphs' :: Parser [ParaPart]
emphs' =
  withinBalancing Emph
  emphMarkerLeft
  emphMarkerRight
  (contentsWithinEmphsWith' manyParaParts)

bolds' :: Parser [ParaPart]
bolds' =
  withinBalancing Bold
  boldMarkerLeft
  boldMarkerRight
  (contentsWithinBoldsWith' manyParaParts)

manyParaParts :: Parser [ParaPart]
manyParaParts = some $ try paraPart

-- make contents within matching punctuation; the prts parameter determines
-- how to parse the ParaParts within brackets, parentheses etc.

contentsWithinBracksWith' :: Parser [ParaPart] -> Parser [ParaPart]
contentsWithinBracksWith' prts = concat <$>
  many (try $
        try parens'
        <|> try doubles'
        <|> try quotes'
        <|> try emphs'
        <|> try bolds'
        <|> prts
       )

contentsWithinParensWith' :: Parser [ParaPart] -> Parser [ParaPart]
contentsWithinParensWith' prts =  concat <$>
  many (try $
        try bracks'
        <|> try doubles'
        <|> try quotes'
        <|> try emphs'
        <|> try bolds'
        <|> prts
       )

contentsWithinDoublesWith' :: Parser [ParaPart] -> Parser [ParaPart]
contentsWithinDoublesWith' prts =  concat <$>
  many (try $
        try bracks'
        <|> try parens'
        <|> try quotes'
        <|> try emphs'
        <|> try bolds'
        <|> prts
       )

contentsWithinQuotesWith' :: Parser [ParaPart] -> Parser [ParaPart]
contentsWithinQuotesWith' prts = concat <$>
  many (try $
        try bracks'
        <|> try parens'
        <|> try doubles'
        <|> try emphs'
        <|> try bolds'
        <|> prts
       )

contentsWithinEmphsWith' :: Parser [ParaPart] -> Parser [ParaPart]
contentsWithinEmphsWith' prts = concat <$>
  many (try $
        try bracks'
        <|> try parens'
        <|> try doubles'
        <|> try quotes'
        <|> try bolds'
        <|> prts
       )

contentsWithinBoldsWith' :: Parser [ParaPart] -> Parser [ParaPart]
contentsWithinBoldsWith' prts = concat <$>
   many (try $
        try bracks'
        <|> try parens'
        <|> try doubles'
        <|> try quotes'
        <|> try emphs'
        <|> prts
       )



--paraPart

someParaPart :: Parser ParaPart
someParaPart = paraPart

paraPartWith :: Parser Inline -> Parser ParaPart
paraPartWith pinl =  (try (footnoteP)
           <|> try (paraComment)
           <|> try (sentenceWith' eosPunctNoFollowUp)
           <|> inlinesNonempty) <* pinl

-- A part of a paragraph (paraPart) is a footnote, comment, sentence or list of
-- inlines that is followed by either a strict newline or a marker or a parend.
paraPart :: Parser ParaPart
paraPart = paraPartWith afterEnd
  where
    afterEnd = try newlnStrict
           <|> try (lookAhead parend)
           <|> try ( (lookAhead marker *> return Null))
           <|> return Null

--an in-paragraph footnote: ParaFtn Int [Sentence]; cf. footnoteB
-- you should allow for Inlines and styling
footnoteP :: Parser ParaPart
footnoteP = ParaFtn 0 <$> smallftn <?> "footnoteP"



--Sentence parser (auxiliary parsers defined in Inlines module)

sentence :: Parser Sentence
sentence = sentenceWith eosPunctNoFollowUp



--comments in para: ParaComment [ParaPart]

paraComment :: Parser ParaPart
paraComment = fmap ParaComment $ openComment *> some (try someParaPart) <* closeComment

