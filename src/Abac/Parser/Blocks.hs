module Abac.Parser.Blocks where

import qualified Data.Text as T

import Data.Monoid ((<>))
import Control.Applicative hiding ((<|>))
import Data.Functor (($>))

import Text.Megaparsec
import Text.Megaparsec.Char

import Abac.Types.ParserTypes
import Abac.Internal (posFromSource)


import Abac.Parser.Operations
import Abac.Parser.Internal
import Abac.Parser.ParaParts
import Abac.Parser.Examples
import Abac.Parser.Inlines
import Abac.Parser.Tech


emptyBlockQuote :: Parser Block
emptyBlockQuote =
  let fin = parend $> () <|> eof
  in lexeme (string ">") *> fin *> pure (BlockQuotes [])


--blocks: we include versions of parsers that assume no particular block-delimiter (like "\n\n").
--We can call these 'bare blocks', and we add a prime (') on the parser that produces them.
--Bare blocks are needed to define blocks inside (larger) yaml blocks. (The primed versions
--are given in the Yaml module)

block :: Parser Block
block = blockWith parend

blockWith :: Parser Inline -> Parser Block
blockWith end =
  (try linkref
  <|> try imageref
  <|> try paragraph
  <|> try footnoteB
  <|> try blockex
  <|> try blockquote
  <|> try blocktech
  <|> blockComment)
  <* end
  <?> "block"

--Question: what does ensure that paragraphs are not starting with any of these:
--"(@) (2) - + > $$ \[ "?


paragraph :: Parser Paragraph
paragraph = paragraphWith (lookAhead parend)

paragraphWith :: Parser Inline -> Parser Paragraph
paragraphWith end = (fmap Para $ (try decoratedParaPartsNonempty <|> emptyParaPart) <* end) <?> "paragraph"

emptyParaPart :: Parser [Inlines]
emptyParaPart = fmap ((:[]) . Inlines) $
  some $ try emptyParens
    <|> try emptyBrackets
    <|> try emptySngQuotes
    <|> emptyDblQuotes

--a (big) footnote contains paragraphs or other blocks, e.g. examples and quotes

footnoteB :: Parser FootnoteB
footnoteB = footnoteBWith (lookAhead parend)

footnoteBWith :: Parser Inline -> Parser FootnoteB
footnoteBWith end = footnoteBFrom (blockWith $ lookAhead parend) <* end <?> "footnoteB"

footnoteBFrom :: Parser Paragraph -> Parser FootnoteB
footnoteBFrom par = mkFootnote =<< (ftnMarker *> blcks)
  where
    ftnMarker = (string "[^") *> many alphaNumChar <* (string "]:" :: Parser String)
    blcks = (:) <$> par <*> embeddedBlocks
    embeddedBlocks = try (some (try embeddedBlock)) <|> return []
    embeddedBlock = parend *> blockWith (lookAhead parend)
    mkFootnote s = return $ Footnote 0 s



--NB: the footnote parsers are provisional; a footnote may contain brackets as punctuation, which
--makes the `sentence` parser eat up the footnote markers, especially the "]". We need to account
--for the presence of non-marker brackets. For now, brackets are not allowed in footnotes.

--BlockQuote Paragraph: only lazy blockquotes are supported

blockquote :: Parser Block
blockquote = try emptyBlockQuote <|> blockquoteLevWith 1 (lookAhead parend)

-- lists of items and examples in a blockquote need special treatment; since an example marker looks
-- like this "\n(@1)", the blockquote marker ">" be must followed by a newline and the rest of the
-- example marker; this is ugly and should be changed.

blockquoteLevWith :: Int -> Parser Inline -> Parser Block
blockquoteLevWith lev end = do
  let gt = lexeme $ string ">"
  let bqmrk = iterate (<> gt) gt !! (lev - 1) <* notFollowedBy (try gt)
  let firstBlock = bqmrk *> blockWith (lookAhead parend)
  let nextBlocks = many $ try (parend *> blockquoteLevWith lev end)
  let firstLevBlock = fmap BlockQuotes $ (:) <$> firstBlock <*> nextBlocks
  let embeddedBlock = try (parend *> blockquoteLevWith (lev + 1) end) <|> return (Para [])
  embedBlocks' firstLevBlock embeddedBlock <* end
  where
    embedBlocks' :: Parser Block -> Parser Block -> Parser Block
    embedBlocks' host guest = do
      BlockQuotes hblcs <- host
      guestBlock <- guest
      if guestBlock == Para []
         then return $ BlockQuotes hblcs
         else return $ BlockQuotes $ hblcs ++ [guestBlock]


--BlockComment [Block]

blockComment :: Parser Block
blockComment = blockCommentWith (try parend <|> lookAhead closeComment $> Null)

blockCommentWith :: Parser Inline -> Parser Block
blockCommentWith end = fmap BlockComment $
  openComment *>
  many (try $ blockWith end)
  <* closeComment <* end

--BlockTech Tech Text

blocktech :: Parser Block
blocktech = blocktechWith (lookAhead parend)

blocktechWith :: Parser Inline -> Parser Block
blocktechWith end = blocktech' <* end <?> "blocktech" -- try blockmath <|> blockcode


blockmath :: Parser Block
blockmath = blockmath' <* parend <?> "blockmath"

blockcode :: Parser Block
blockcode = blockcode' <* parend <?> "blockcode"

--tech

blocktech' :: Parser Block
blocktech' = try blockmath' <|> blockcode' <?> "blocktech'"

blockmath' :: Parser Block
blockmath' = do
  mathStr <- try bracketBlock <|> ddollarBlock <?> "blockmath'"
  pos <- posFromSource (length mathStr) <$> getPosition
  return $ BlockTech Math pos (T.pack mathStr)
  where
    bracketBlock = space *> mathString openMathBlock closeMathBlock
    ddollarBlock = space *> mathString ddollar ddollar


blockcode' :: Parser Block
blockcode' = do
  codeStr <- try tildeBlock <|> backtickBlock
  pos <- posFromSource (length codeStr) <$> getPosition
  return $ BlockTech Code (fst pos - 1, 0) (T.pack codeStr)
  <?> "blockcode'"
  where
    tildeBlock = between tildes tildes (many $ noneOf ['~'])
    backtickBlock = between backticks backticks (many $ noneOf ['`'])
    tilde = char '~' :: Parser Char
    backtick = char '`' :: Parser Char
    tildes =  tilde >> tilde >> tilde >> many tilde
    backticks = backtick >> backtick >> backtick >> many backtick




--tables

--We don't need to care about alignment; we just need to identify the words correctly.
--Each line of the table is a list of inlines, including end-of-sentence punctuation.

multilineTable :: Parser Table
multilineTable = do
  headerInlines <- maybe [] id <$> optional header
  dashedLineWithSpaces
  rowInlines <- some row <* dashedLine
  captionParaParts <- maybe [] (:[]) <$> optional tableCaption
  return $ Table (headerInlines ++ rowInlines ++ captionParaParts)

dashedLine :: Parser Text
dashedLine = T.pack <$> dash `someTill` (try parend <|> newline $> Newline)

dashedLineWithSpaces :: Parser Text
dashedLineWithSpaces = T.pack <$> (dash <|> ws) `someTill` newline

headerRow :: Parser Inlines -- i.e. ParaPart
headerRow = Inlines <$> many inlineSansNln <* newline

header :: Parser [Inlines]
header = dashedLine *> some headerRow <* lookAhead dashedLineWithSpaces

row :: Parser ParaPart
row = Inlines <$> some (try inline <|> eosPunct) <*
  (try parend <|> lookAhead dashedLine $> ParEnd)

tableCaption :: Parser Caption -- Caption [ParaPart]
tableCaption = Caption <$> (captionCheck *> many someParaPart <* parend)
  where
    captionCheck = lookAhead $ string "Table" >> char ':'

