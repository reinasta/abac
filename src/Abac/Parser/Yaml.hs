module Abac.Parser.Yaml where

import Data.Char hiding (Space)
import qualified Data.Text as T
import Prelude hiding (Word,seq)

import Data.Functor (($>))
import Control.Applicative hiding ((<|>),many,some)
import qualified Data.Map.Strict as M

import Text.Megaparsec
import Text.Megaparsec.Char


import Abac.Types.ParserTypes

import Abac.Parser.Operations
import Abac.Parser.Internal
import Abac.Parser.Inlines
import Abac.Parser.Tech
import Abac.Parser.ParaParts
import Abac.Parser.Blocks
import Abac.Parser.Examples


--The 'unprimed' parsers include the end-delimiter (namely, two newlines) at the end of each block.

block' :: Parser Block
block' = notFollowedBy yamlPair *>
  (try paragraph'
  <|> try footnoteB'
  <|> try blockex'
  <|> try blockquote'
  <|> try blocktech'
  <|> blockComment' <* lookAhead newline)
  <?> "block'"

blockex' :: Parser Block
blockex' = blockex

--otherBlock :: Parser Block
--otherBlock = choice [ordered, unordered, blockquote, blocktech, blockComment]

--otherBlock' :: Parser Block
--otherBlock' = choice [ordered', unordered', blockquote', blocktech',blockComment']


paragraph' :: Parser Paragraph
paragraph' = Para <$> (some $ try someParaPart') <?> "paragraph'"


someParaPart' :: Parser ParaPart
someParaPart' = (try (lexeme footnoteP)
            <|> try (lexeme paraComment')
            <|> try sentence'
            <|> inlines'
            <?> "someParaPart")

-- old blockquote parsers that are kept for yaml sections for now

--blockquoteWith :: Parser Inline -> Parser Block
--blockquoteWith end = blockquoteFrom paragraph <* end <?> "blockquote"

blockquote' :: Parser Block
blockquote' = blockquoteFrom paragraph' <?> "blockquote'"

blockquoteFrom :: Parser Paragraph -> Parser Block
blockquoteFrom par = fmap BlockQuote $ (lexeme $ char '>') *> par

-- comments: inline and block

paraComment' :: Parser ParaPart
paraComment' = fmap ParaComment $ openComment *> some (try someParaPart') <* closeComment


blockComment' :: Parser Block
blockComment' = fmap BlockComment $ openComment *> many (try block') <* closeComment

-- para parts

footnoteB' :: Parser FootnoteB
footnoteB' = footnoteBFrom paragraph' <?> "footnoteB'"


sentence' :: Parser Sentence
sentence' = Sentence <$> do
  inlns <- some $ try inlineSansNln
  pct <-  eosPunct
  return $ inlns ++ [pct]
  <?> "yaml-sentence"

inline' :: Parser Inline
inline' = choice [insSpace, num, word, insPunct, citation, techInline, inlineComment] <?> "inline'"


inlines' :: Parser ParaPart
inlines' = Inlines <$> some inline'

inlineComment' :: Parser Inline
inlineComment' = fmap InlineComment $ openComment *> many (try inline') <* closeComment


--yaml parser

yaml :: Parser Meta
yaml = fmap Meta $ begDelimiter *> (M.fromList <$> many yamlPair) <* endDelimiter

begDelimiter, endDelimiter :: Parser String
begDelimiter = str "---" <* newline
endDelimiter = (try (str "---") <|> str "...") <* (many newline $> () <|> eof)

yamlPair :: Parser (MetaKey, MetaValue)
yamlPair = liftA2 (,) metavar metaval
  where
    metavar = some alphaNumChar <* (lexeme $ char ':') >>= stringToVar
    metaval = choice [seq, date, inls, bool, blcks]

    seq = let sep = str "\n-" in MetaSeq <$> (sep *> many inlineSansNln `sepBy` sep <* newln)
    date = let sep = oneOf (" -" :: String) in MetaNum <$>
      (fmap read <$> (many numberChar `sepBy` sep <* newln) :: Parser [Int])
    inls = fmap MetaInlines $ optBracket *> many (try inlineSansNln <|> eosPunct)
      <* optional (char ']') <* newln
    bool = MetaBool <$>
      (fmap read $ try (string' "True") <|> string' "False" :: Parser Bool) <* newln
    blcks = MetaBlocks <$> (try (lexeme (char '|') >> newline) >> blocksYaml)

    optBracket = optional $ notFollowedBy link *> char '['

    stringToVar s =
      return $ case (map toLower s) of
      "author"   -> AuthorKey
      "title"    -> TitleKey
      "date"     -> DateKey
      "abstract" -> AbstractKey
      "tags"     -> TagKey
      _          -> OtherKey $ T.pack s

    blocksYaml  :: Parser [Block]
    blocksYaml = some $ lexeme block' <* parendYaml

    parendYaml :: Parser ()
    parendYaml = try yamlobj <|> newlines
      where
        newlines = newln >> skipMany (char ' ') >> newln >> skipMany newln
        yamlobj = newln >> lookAhead (try endDelimiter <|> yamlPair $> "pair yeah!") $> ()

str :: String -> Parser String
str = lexeme . string

