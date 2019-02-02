module Abac.Parser.Inlines where

import Data.Char hiding (Space)
import qualified Data.Text as T
import Data.Functor (($>))
import Data.Monoid ((<>))
import Control.Applicative hiding ((<|>),many,some)
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding (Word,id)


import Abac.Types.ParserTypes
import Abac.Internal (posFromSource)
import Abac.Traverse (Searchable,addAttr, addEmph, addBold)
import Abac.Parser.Operations
import Abac.Parser.Internal
import Abac.Parser.Markers
import Abac.Parser.Tech


-- Dealing with the quote/apostrophe ambiguity (decoratedInlines)
--  1. assume all in-word quotes (e.g. "Lewis's", "brother's" ) are apostrophes, and thus are
--     part of the word rather than external to it
--  2. for strings ending in a quote-like character,
--       a. first, if the word's final letter is "s", parse the quote as an apostrophe;
--       b. then try to parse the quote as a right quote
--       c. apply the more fruitful strategy (parser) in terms of words parsed


-- parse empty matching punctuation
emptyMatching :: Parser String -> Parser String -> Attr -> Parser Inline
emptyMatching opn cls attr = do
  opn
  cls
  pos <- posFromSource 1 <$> getSourcePos
  return $ Word [attr] pos T.empty

emptyParens :: Parser Inline
emptyParens =
  let opn = string "("
      cls = string ")"
  in  emptyMatching opn cls Parenthetical

emptyBrackets :: Parser Inline
emptyBrackets =
  let opn = string "["
      cls = string "]"
  in  emptyMatching opn cls Bracketed

emptySngQuotes :: Parser Inline
emptySngQuotes =
  let opn = string "\'"
      cls = string "\'"
  in  emptyMatching opn cls Quoted

emptyDblQuotes :: Parser Inline
emptyDblQuotes =
  let opn = string "\""
      cls = string "\""
  in  emptyMatching opn cls DoubleQuoted




-- most general parser for inlines, includes all matching punctuation, e.g. parens, brackets etc.
-- note that it depends on inlinesSans
decoratedInlines :: Parser [Inline]
decoratedInlines = try (eof *> return []) <|> do
  notFollowedBy $ try parend <|> sectionMarker $> Null
  inls' <- lookAhead decoratedInlinesApostr
  inls'' <- lookAhead decoratedInlinesNoApostr
  if length inls'' > length inls'
    then decoratedInlinesNoApostr
    else decoratedInlinesApostr

-- apostrophe version
decoratedInlinesApostr :: Parser [Inline]
decoratedInlinesApostr = fmap concat $
  many $
    try bracksApostrophe
    <|> try parensApostrophe
    <|> try doublesApostrophe
    <|> try quotesApostrophe
    <|> try emphsApostrophe
    <|> try boldsApostrophe
    <|> manyInlinesApostropheExternal


-- no apostrophe version
decoratedInlinesNoApostr :: Parser [Inline]
decoratedInlinesNoApostr = fmap concat $
  many $
    try bracksNoApostrophe
    <|> try parensNoApostrophe
    <|> try doublesNoApostrophe
    <|> try quotesNoApostrophe
    <|> try emphsNoApostrophe
    <|> try boldsNoApostrophe
    <|> manyInlinesNoApostropheExternal


-- generic function to produce parsers for balancing punctuation
withinBalancing :: (Searchable a) => Attr -> Parser String -> Parser String -> Parser [a] -> Parser [a]
withinBalancing attr open close contents = do
  lexeme open
  as1 <- contents
  as2 <- againWithContents
  lexeme close
  as5 <- try again <|> return []
  return $ addAttr attr <$> as1 ++ as2 ++ as5
  where
    againWithContents =
      let repeat' = fmap concat $ some $ try (lookAhead open *> againWithContents')
      in  try repeat' <|> return []
    again = withinBalancing attr open close contents
    againWithContents' = go' again contents
    go' :: Parser [a] -> Parser [a] -> Parser [a]
    go' f cont = do
      match' <- f -- try f <|> return []
      after <- cont
      return (match' ++ after)



-- parser for balancing punctuation ('apostrophe' versions)

bracksApostrophe :: Parser [Inline]
bracksApostrophe =
  withinBalancing Bracketed
    leftBracket
    rightBracket
    contentsWithinBracksApostrophe

parensApostrophe :: Parser [Inline]
parensApostrophe =
  withinBalancing Parenthetical
    leftParen
    rightParen
    contentsWithinParensApostrophe

doublesApostrophe :: Parser [Inline]
doublesApostrophe =
  withinBalancing DoubleQuoted
    leftDoubleQuote
    rightDoubleQuote
    (contentsWithinDoublesApostrophe)

quotesApostrophe :: Parser [Inline]
quotesApostrophe =
  withinBalancing Quoted
    leftQuote
    rightQuote
    contentsWithinQuotesApostrophe

emphsApostrophe :: Parser [Inline]
emphsApostrophe =
  withinBalancing Emph
  emphMarkerLeft
  emphMarkerRight
  contentsWithinEmphsApostrophe

boldsApostrophe :: Parser [Inline]
boldsApostrophe =
  withinBalancing Bold
  boldMarkerLeft
  boldMarkerRight
  contentsWithinBoldsApostrophe

manyInlinesApostrophe :: Parser [Inline]
manyInlinesApostrophe = some $ try inlineSansBalancing'

manyInlinesApostropheExternal :: Parser [Inline]
manyInlinesApostropheExternal = some $ try inlineSansBalancingExt'


-- parser for balancing punctuation ('no apostrophe' versions)

bracksNoApostrophe :: Parser [Inline]
bracksNoApostrophe =
  withinBalancing Bracketed
    leftBracket
    rightBracket
    contentsWithinBracksNoApostrophe

parensNoApostrophe :: Parser [Inline]
parensNoApostrophe =
  withinBalancing Parenthetical
    leftParen
    rightParen
    contentsWithinParensNoApostrophe

doublesNoApostrophe :: Parser [Inline]
doublesNoApostrophe =
  withinBalancing DoubleQuoted
    leftDoubleQuote
    rightDoubleQuote
    contentsWithinDoublesNoApostrophe

quotesNoApostrophe :: Parser [Inline]
quotesNoApostrophe =
  withinBalancing Quoted
    leftQuote
    rightQuote
    contentsWithinQuotesNoApostrophe

emphsNoApostrophe :: Parser [Inline]
emphsNoApostrophe =
  withinBalancing Emph
  emphMarkerLeft
  emphMarkerRight
  contentsWithinEmphsNoApostrophe

boldsNoApostrophe :: Parser [Inline]
boldsNoApostrophe =
  withinBalancing Bold
  boldMarkerLeft
  boldMarkerRight
  contentsWithinBoldsNoApostrophe

manyInlinesNoApostrophe :: Parser [Inline]
manyInlinesNoApostrophe = some $ try inlineSansBalancing''

manyInlinesNoApostropheExternal :: Parser [Inline]
manyInlinesNoApostropheExternal = some $ try inlineSansBalancingExt''


-- parse punctuation so that single quotation marks at the end words whose final
-- letter is "s" are considered as apostrophes and thus part of the words (rather than quotations)

contentsWithinBracksApostrophe :: Parser [Inline]
contentsWithinBracksApostrophe = contentsWithinBracksApostrWith manyInlinesApostrophe

contentsWithinParensApostrophe :: Parser [Inline]
contentsWithinParensApostrophe = contentsWithinParensApostrWith manyInlinesApostrophe

contentsWithinDoublesApostrophe :: Parser [Inline]
contentsWithinDoublesApostrophe = contentsWithinDoublesApostrWith manyInlinesApostrophe

contentsWithinQuotesApostrophe :: Parser [Inline]
contentsWithinQuotesApostrophe = contentsWithinQuotesApostrWith manyInlinesApostrophe

contentsWithinEmphsApostrophe :: Parser [Inline]
contentsWithinEmphsApostrophe = contentsWithinEmphsApostrWith manyInlinesApostrophe

contentsWithinBoldsApostrophe :: Parser [Inline]
contentsWithinBoldsApostrophe = contentsWithinBoldsApostrWith manyInlinesApostrophe

-- parse punctuation so that single quotation marks at the end words are considered
-- quotations and so not part of those words

contentsWithinBracksNoApostrophe :: Parser [Inline]
contentsWithinBracksNoApostrophe = contentsWithinBracksNoApostrWith manyInlinesNoApostrophe

contentsWithinParensNoApostrophe :: Parser [Inline]
contentsWithinParensNoApostrophe = contentsWithinParensNoApostrWith manyInlinesNoApostrophe

contentsWithinDoublesNoApostrophe :: Parser [Inline]
contentsWithinDoublesNoApostrophe = contentsWithinDoublesNoApostrWith manyInlinesNoApostrophe

contentsWithinQuotesNoApostrophe :: Parser [Inline]
contentsWithinQuotesNoApostrophe = contentsWithinQuotesNoApostrWith manyInlinesNoApostrophe

contentsWithinEmphsNoApostrophe :: Parser [Inline]
contentsWithinEmphsNoApostrophe = contentsWithinEmphsNoApostrWith manyInlinesNoApostrophe

contentsWithinBoldsNoApostrophe :: Parser [Inline]
contentsWithinBoldsNoApostrophe = contentsWithinBoldsNoApostrWith manyInlinesNoApostrophe



-- make contents within matching punctuation; the inls parameter determines
-- how to parse the inlines within brackets, parentheses etc.
-- this version parses words with final apostrophes if the preceding letter is 's'

contentsWithinBracksApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinBracksApostrWith inls = concat <$>
  many (try $
        try parensApostrophe
        <|> try doublesApostrophe
        <|> try quotesApostrophe
        <|> try emphsApostrophe
        <|> try boldsApostrophe
        <|> inls
       )

contentsWithinParensApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinParensApostrWith inls =  concat <$>
  many (try $
        try bracksApostrophe
        <|> try doublesApostrophe
        <|> try quotesApostrophe
        <|> try emphsApostrophe
        <|> try boldsApostrophe
        <|> inls
       )

contentsWithinDoublesApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinDoublesApostrWith inls =  concat <$>
  many (try $
        try bracksApostrophe
        <|> try parensApostrophe
        <|> try quotesApostrophe
        <|> try emphsApostrophe
        <|> try boldsApostrophe
        <|> inls
       )

contentsWithinQuotesApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinQuotesApostrWith inls = concat <$>
  many (try $
        try bracksApostrophe
        <|> try parensApostrophe
        <|> try doublesApostrophe
        <|> try emphsApostrophe
        <|> try boldsApostrophe
        <|> inls
       )

contentsWithinEmphsApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinEmphsApostrWith inls = concat <$>
  many (try $
        try bracksApostrophe
        <|> try parensApostrophe
        <|> try doublesApostrophe
        <|> try quotesApostrophe
        <|> try boldsApostrophe
        <|> inls
       )

contentsWithinBoldsApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinBoldsApostrWith inls = concat <$>
   many (try $
        try bracksApostrophe
        <|> try parensApostrophe
        <|> try doublesApostrophe
        <|> try quotesApostrophe
        <|> try emphsApostrophe
        <|> inls
       )

-- make contents within matching punctuation; the inls parameter determines
-- how to parse the inlines within brackets, parentheses etc.
-- this version parses words without final apostrophes

contentsWithinBracksNoApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinBracksNoApostrWith inls = concat <$>
  many (try $
        try parensNoApostrophe
        <|> try doublesNoApostrophe
        <|> try quotesNoApostrophe
        <|> try emphsNoApostrophe
        <|> try boldsNoApostrophe
        <|> inls
       )

contentsWithinParensNoApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinParensNoApostrWith inls =  concat <$>
  many (try $
        try bracksNoApostrophe
        <|> try doublesNoApostrophe
        <|> try quotesNoApostrophe
        <|> try emphsNoApostrophe
        <|> try boldsNoApostrophe
        <|> inls
       )

contentsWithinDoublesNoApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinDoublesNoApostrWith inls =  concat <$>
  many (try $
        try bracksNoApostrophe
        <|> try parensNoApostrophe
        <|> try quotesNoApostrophe
        <|> try emphsNoApostrophe
        <|> try boldsNoApostrophe
        <|> inls
       )

contentsWithinQuotesNoApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinQuotesNoApostrWith inls = concat <$>
  many (try $
        try bracksNoApostrophe
        <|> try parensNoApostrophe
        <|> try doublesNoApostrophe
        <|> try emphsNoApostrophe
        <|> try boldsNoApostrophe
        <|> inls
       )

contentsWithinEmphsNoApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinEmphsNoApostrWith inls = concat <$>
  many (try $
        try bracksNoApostrophe
        <|> try parensNoApostrophe
        <|> try doublesNoApostrophe
        <|> try quotesNoApostrophe
        <|> try boldsNoApostrophe
        <|> inls
       )

contentsWithinBoldsNoApostrWith :: Parser [Inline] -> Parser [Inline]
contentsWithinBoldsNoApostrWith inls = concat <$>
   many (try $
        try bracksNoApostrophe
        <|> try parensNoApostrophe
        <|> try doublesNoApostrophe
        <|> try quotesNoApostrophe
        <|> try emphsNoApostrophe
        <|> inls
       )



-- new inline, without balancing punctuation "()[]''" etc. but with words allowing for final apostrophes
inlineSansBalancing :: Parser Inline
inlineSansBalancing = (
  choice [ insSpace
         , newlnStrict
         , num
         , word
         , link
         , image
         , insPunctSansBalancing
         , email
         , sentFtn
         , citation
         , techInline
         , inlineComment
         ] <?> "inlineSansBalancing" )



-- new inline, without balancing punctuation "()[]''" etc.; with words allowing for final apostrophes
-- only if they are possessives ending in 's'
-- internal version: it parses end-of-sentence punctuation, as e.g. quotes may contain full sentences
inlineSansBalancing' :: Parser Inline
inlineSansBalancing' = {- dbg "inlineSansBalancing'" -} (
  choice [ insSpace
         , newlnStrict
         , try num
         , wordWithApostrophe
         , link
         , try image
         , insPunctSansBalancing
         , email
         , sentFtn
         , citation
         , techInline
         , inlineComment
         , eosPunctNoFollowUp
         ] <?> "inlineSansBalancing'" )

-- inline including words without apostrophes;
-- 'internal' version: includes end-of-sentence punctuation as well (as full sentences can be quoted)
inlineSansBalancing'' :: Parser Inline
inlineSansBalancing'' = (
  choice [ insSpace
         , newlnStrict
         , try num
         , wordSansApostrophe
         , link
         , image
         , insPunctSansBalancing
         , email
         , sentFtn
         , citation
         , techInline
         , inlineComment
         , eosPunctNoFollowUp
         ] <?> "inlineSansBalancing''" )


-- 'external' verion of the above: does not include end-of-sentence punctuation
inlineSansBalancingExt' :: Parser Inline
inlineSansBalancingExt' = {- dbg "inlineSansBalancing'" -} (
  choice [ insSpace
         , newlnStrict
         , try num
         , wordWithApostrophe
         , link
         , image
         , insPunctSansBalancing
         , email
         , sentFtn
         , citation
         , techInline
         , inlineComment
         ] <?> "inlineSansBalancing'" )

-- external version of the above: does not include end-of-sentence punctuation
inlineSansBalancingExt'' :: Parser Inline
inlineSansBalancingExt'' = (
  choice [ insSpace
         , newlnStrict
         , try num
         , wordSansApostrophe
         , link
         , image
         , insPunctSansBalancing
         , email
         , sentFtn
         , citation
         , techInline
         , inlineComment
         ] <?> "inlineSansBalancing''" )




insPunctSansBalancing :: Parser Punctuation -- Punct PunctType Text
insPunctSansBalancing = (Punct InSentence . T.pack) <$>
  (noDash4x *> noCommentDashes *> (try noncitPunct <|> citPunct)) <?> "insPunct"
  where
    -- punctuation chars are divided into those that can appear in
    -- reference inlines (citations) and those that don't
    noncitPunct = some $ oneOf ( ";,+-–\8211\8212" :: String)
    noCommentDashes = notFollowedBy $ char '-' >> char '-' >> char '>'
    citPunct = try inlineCol <|> inlineDash
    inlineCol = string ":" <* lookAhead (wsp <|> eof)
    inlineDash = lookAhead wsp *> some (char '-') <* lookAhead wsp
    wsp = spaceChar $> ()
    -- groups of more than three dashes are not considered punctuation
    noDash4x = notFollowedBy $ char '-' >> char '-' >> char '-' >> char '-'

-- NB
-- emdash                    \8212
-- endash                    \8211
-- open curly quotes         \8216
-- close curly quotes        \8217
-- open double curly quotes  \8220
-- close double curly quotes \8221


-- any punct
punct :: Parser Punctuation
punct = try insPunct <|> eosPunct

mkEosPunct :: Parser Char -> Parser Punctuation -- Punct PunctType Text
mkEosPunct additionalPunct = (Punct EndSentence . T.pack) <$>
  some regularPunct <> many (try $ noClosingComment >> noTag >> additionalPunct)
 where
   exclam = char '!' <* notFollowedBy (char '[' <|> char '-')  -- cf. images "![", comments "<!--"
   fullst = char '.' <* notFollowedBy alphaNumChar -- cf. 0.5, name@gmail.com
   regularPunct = try fullst <|> try exclam <|> char '?'
   noClosingComment = notFollowedBy $ string "-->"
   noTag = notFollowedBy $ char '/'

eosPunct :: Parser Punctuation -- Punct PunctType Text
eosPunct = (mkEosPunct punctuationChar <?> "eosPunct")


eosPunctNoFollowUp :: Parser Punctuation -- Punct PunctType Text
eosPunctNoFollowUp = (Punct EndSentence . T.pack) <$> some regularPunct
 where
   exclam = char '!' <* notFollowedBy (char '[' <|> char '-')  -- cf. images "![", comments "<!--"
   fullst = char '.' <* notFollowedBy alphaNumChar -- cf. 0.5, name@gmail.com
   regularPunct = try fullst <|> try exclam <|> char '?'




eosPunct' :: Parser Punctuation
eosPunct' = mkEosPunct rightPunct

rightPunct :: Parser Char
rightPunct = char '\'' <|> char '\8217' <|> char '\34' <|> char '\8221' <|> char '\41' <|>
  char '\187' <|> char '\8216' <|> char '\8223' <|> char '\1075' <|> char '\1076'

--Some closing punctuation in unicode decimal codes. Note that the closing bracket ']'
--is not on the list, as it is used as closing marker for short footnotes. For a more
--complete list of character codes see:
--http://unicodelookup.com/#quotation/1
--http://unicodelookup.com/#parenthesis/1

insPunct :: Parser Punctuation -- Punct PunctType Text
insPunct = (Punct InSentence . T.pack) <$>
  (noDash4x *> noCommentDashes *> (try noncitPunct <|> citPunct)) <?> "insPunct"
  where
    -- punctuation chars are divided into those that can appear in
    -- reference inlines (citations) and those that don't
    noncitPunct = some $ oneOf ( "\"\';,()+-–\8216\8217\8220\8221" :: String)
    noCommentDashes = notFollowedBy $ char '-' >> char '-' >> char '>'
    citPunct = try inlineCol <|> inlineDash
    inlineCol = string ":" <* lookAhead (wsp <|> eof)
    inlineDash = lookAhead wsp *> some (char '-') <* lookAhead wsp
    wsp = spaceChar $> ()
    -- groups of more than three dashes are not considered punctuation
    noDash4x = notFollowedBy $ char '-' >> char '-' >> char '-' >> char '-'

insSpace :: Parser Inline -- Space
insSpace = some (char ' ') >> return Space <?> "insSpace"

optSpace :: Parser Inline
optSpace = fromMaybe Null <$> (optional insSpace)


newln  :: Parser Inline -- Newline
newln = newline >> return Newline <?> "newln"

{- Idea: modify parend to consume just one newline when it finds a parend
followed by an `example`. It will perhaps do to redefine `noMarker` as:

       notFollwedBy $ many insSpace >> anyMarker

because then the newline (oneNl) before any marker will not be consumed.
-}

parend :: Parser Inline
parend = ( (try newlines
  <|> try endInComment
  <|> try newlinePlusMarker
  <|> endOfFile)
  *> return ParEnd
  <?> "parend" )
  where
    newlines = oneNl *> oneNl *> many oneNl *> return ()
    endInComment = many newline *> return () <* lookAhead (string "-->")
    oneNl = newln <* noExMarker <* skipMany (char ' ')
    noExMarker = notFollowedBy $ many (char ' ') *> marker
    nlBeforeMarker = newln <* lookAhead marker
    newlinePlusMarker = many (try oneNl) *> nlBeforeMarker *> return ()
    --newlinePlusMarker = newline *> lookAhead mrk *> return ()
    endOfFile = space >> eof :: Parser ()




--An emphasized word is of the form *word, or word*, or occupies a place between
--words of such forms.

styledInlines :: Parser [Inline]
styledInlines = fmap concat $ many $ try $ try emphWords <|> try boldWords <|> inlineLifted
  where
    inlineLifted = fmap (:[]) inline

emphWords :: Parser [Inline]
emphWords = emphasize $ emphMarkers (many $ try inline)
  where
    emphasize = (fmap . fmap) addEmph
    emphMarkers = between emphMarkerLeft emphMarkerRight

boldWords :: Parser [Inline]
boldWords = mkbold $ boldMarkers (many $ try inline)
  where
    mkbold = (fmap . fmap) addBold
    boldMarkers = between boldMarkerLeft boldMarkerRight



--various markers

openFtnP, closeFntP, openFtnB, closeFntB, closeFntB' :: Parser String
openFtnP = string "^["
closeFntP = string "]"
openFtnB = string "[^"
closeFntB = string "]:"
closeFntB' = string "]" -- closing marker for a reference to a big footnote

noMarker :: Parser ()
noMarker = notFollowedBy endOfExample

endOfExample :: Parser ()
endOfExample =
  choice [ marker $> ()
         , newline $> ()
         , eof
         ] <?> "endOfExample"


--note: this is type build by Inlines [Inline] rather than the [Inline] type

inlines :: Parser ParaPart
inlines = fmap Inlines $ noMarker *> decoratedInlines <* end
  where
    end = lookAhead $ try parend $> () <|> try closeComment $> () <|> try footnoteMarker $> () <|>
      try (oneOf "\'\")]") $> () <|> eof <|> return ()
    footnoteMarker = try openFtnP <|> openFtnB


inlinesNonempty :: Parser ParaPart
inlinesNonempty = do
  Inlines inls <- inlines
  if null inls then fail "empty inlines" else return (Inlines inls)

-- new version: used in yaml sections and tables
inlineSansNln' :: Parser Inline
inlineSansNln' = undefined


-- used in yaml sections and tables
inlineSansNln :: Parser Inline
inlineSansNln =
  choice [ insSpace
         , link
         , word
         , insPunct
         , email
         , citation
         , techInline
         , num
         ]
  <?> "inlineSansNln"


openComment, closeComment :: Parser String
openComment = lexeme $ string "<!--"
closeComment = lexeme $ string "-->"


--Word [Attr] Text, Citation Int Text, InlineTech Tech Text, etc.


inline :: Parser Inline
inline = (
  choice [ insSpace
         , newlnStrict
         , num
         , word
         , link
         , image
         , insPunct
         , email
         , citation
         , techInline
         , inlineComment
         ] <?> "inline" )

newlnStrict :: Parser Inline
newlnStrict = notFollowedBy someMarker *> newln <* notFollowedBy anotherNewline
  where
    anotherNewline = try (lexeme newline) $> ()
    someMarker = try (lexeme marker) $> ()

num :: Parser Number
num = do
  numStr <- (try numWithPunct <|> numWoPunct) <* notFollowedBy apostr -- "90's" is a word, not a num
  pos <- posFromSource (length numStr) <$> getSourcePos
  return $ Number [None] pos (read numStr)
  where
    numWithPunct = (many numberChar <+ numericPunct) <> numWoPunct
    numWoPunct = some digitChar
    numericPunct = char '.' <|> char ','
    apostr = char '\'' :: Parser Char


--word and citation


-- this version of the word parser incorporates quotation marks within the word's character string
word :: Parser Word
word = do
  wordStr <- (try wordWithDashes <|> try alphaNumWord)
  pos <- posFromSource (length wordStr) <$> getSourcePos
  return $ Word [None] pos (T.pack wordStr)
  <?> "word"
  where
    dash1x = string "-" <* notFollowedBy (char '-')
    dash2x = string "--" <* notFollowedBy (char '-')
    alphaNumWord = some $ (alphaNumChar <* notFollowedBy (try (char '.' <* alphaNumChar) <|> char '@'))
      <|> char '&' <|> quotelike
    wordWithDashes = alphaNumWord <> (try dash1x <|> dash2x) <> many (alphaNumChar <|> quotelike)
    quotelike = char '\'' <|> char '’' <|> char '‘' <|> char '\8242' <|> char '\8243' <|> char '\39'



-- word that ends in 's'followed by a final apostrophe (which is not followed by another letter)
-- quotation marks at the beginning of the word are not allowed; inner quotation marks are allowed
wordWithApostrophe :: Parser Word
wordWithApostrophe = do
  wordStr' <- (try wordWithDashes <|> try alphaNumWord)
  wordStr <- addAnyFinalQuoteTo wordStr'
  pos <- posFromSource (length wordStr) <$> getSourcePos
  return $ Word [None] pos (T.pack wordStr)
  <?> "word"
  where
    wordWithDashes, alphaNumWord :: Parser String
    wordWithDashes = noquote *> alphaNumWord <> (try dash1x <|> dash2x) <> afterDash
    alphaNumWord = noquote *> some (try $ nonEmailOrNumChar <|> char '&' <|> nonFinalQuoteLike)

    dash1x = string "-" <* notFollowedBy (char '-')
    dash2x = string "--" <* notFollowedBy (char '-')
    afterDash = try alphaNumWord <|> return []
    nonEmailOrNumChar = alphaNumChar <* notFollowedBy (try (char '.' <* alphaNumChar) <|> char '@')
    nonFinalQuoteLike = quotelike <* lookAhead letterChar
    quotelike = char '\'' <|> char '’' -- <|> char '‘' <|> char '\8242' <|> char '\8243' <|> char '\39'
    apostrophe = quotelike <* lookAhead (spaceChar $> () <|> punctuationChar $> () <|> eof)
    --quotes = "\'’‘\8242\8243\39"
    noquote = notFollowedBy quotelike

    addAnyFinalQuoteTo :: String -> Parser String
    addAnyFinalQuoteTo str = do
      if last str /= 's'
         then return str
         else do
           mc <- optional apostrophe
           case mc of
             Just _ -> return $ str ++ "\'"
             Nothing -> return str



-- word that cannot end in a (final) apostrophe; the only difference to wordWithApostrophe
-- is that wordSansApostrophe does not allow an apostrophe even if it follows a final 's'
wordSansApostrophe :: Parser Word
wordSansApostrophe = do
  wordStr <- (try wordWithDashes <|> try alphaNumWord)
  pos <- posFromSource (length wordStr) <$> getSourcePos
  return $ Word [None] pos (T.pack wordStr)
  <?> "word"
  where
    wordWithDashes, alphaNumWord :: Parser String
    wordWithDashes = noquote *> alphaNumWord <> (try dash1x <|> dash2x) <> afterDash
    alphaNumWord = noquote *> some (try $ nonEmailOrNumChar <|> char '&' <|> nonFinalQuoteLike)

    dash1x = string "-" <* notFollowedBy (char '-')
    dash2x = string "--" <* notFollowedBy (char '-')
    afterDash = try alphaNumWord <|> return []
    nonEmailOrNumChar = alphaNumChar <* notFollowedBy (try (char '.' <* alphaNumChar) <|> char '@')
    nonFinalQuoteLike = quotelike <* lookAhead letterChar
    quotelike = char '\'' <|> char '’' <|> char '‘' <|> char '\8242' <|> char '\8243' <|> char '\39'
    --apostrophe = quotelike <* lookAhead (spaceChar $> () <|> punctuationChar $> () <|> eof)
    --quotes = "\'’‘\8242\8243\39"
    noquote = notFollowedBy quotelike


-- citations
citation :: Parser Citation
citation = do
  citStr <- (notFollowedBy alphaNumChar *> string "@" *> referenceKey <* optionalRefInfo)
  pos <- posFromSource (length citStr) <$> getSourcePos
  return $ Citation [None] 0 pos (T.pack citStr)
  <?> "citation"
  where
    -- note that apostrophes are included in the reference key (see apos)
    referenceKey = some citKeyChar <> apos <?> "reference key"
    optionalRefInfo :: Parser (Maybe String)
    optionalRefInfo = optional (try $ lexeme (between open close $ many (noneOf [']'])))

    open = char '['
    close = char ']'
    citKeyChar = alphaNumChar <|> oneOf ("-:" :: String)
    quotelike = char '\'' <|> char '’' <|> char '‘' <|> char '\8242' <|> char '\8243' <|> char '\39'
    apos = try (quotelike *> char 's' *> notFollowedBy alphaNumChar *> pure "'s") <|> pure "" :: Parser String

-- email
email :: Parser Email
email = do
  name <- some $ alphaNumChar <|> char '_' <|> char '.'
  pos <- posFromSource (length name) <$> getSourcePos
  char '@'
  domain <- (some alphaNumChar <+ char '.') <> some letterChar
  return $ Email [None] pos (T.pack name) (T.pack domain)
  <?> "email"


-- inline comment
inlineComment :: Parser Inline
inlineComment = fmap InlineComment $ openComment *> many (try inline) <* closeComment

--links & urls

link :: Parser Link
link = do
  inls <- linkText
  urlOrId <- try url' <|> idp
  return $ Link inls urlOrId
  where
    linkText = openBrckt *> many inline <* closeBrckt
    url' = openPrn *> url <* optTitle <* closePrn

url :: Parser Text
url = fmap T.pack $ prefix <> rest
  where
    prefix = try (string "http") <|> try (string "www") <|> string "/"
    rest = manyTill anySingle (char ' ' $> () <|> lookAhead closePrn $> () <|> eof)



linkref :: Parser LinkRef
linkref = linkrefWith (lookAhead parend)

linkrefWith :: Parser Inline -> Parser LinkRef
linkrefWith end = do
  ws3x
  id <- idp <* lexeme (char ':')
  url' <- url
  -- 3 = the char-length of two brackets plus the colon (parsed by the id parser)
  pos <- posFromSource (T.length id + T.length url' + 3) <$> getSourcePos
  optTitle <* end
  return $ LinkRef id pos url'



--images

image :: Parser Image
image = do
  inls <- imageText
  pathOrId <- try pathSansTitle <|> idp
  return $ Image inls pathOrId
  where
    imageText = excl *> openBrckt *> many inline <* closeBrckt
    pathSansTitle = openPrn *> path <* optTitle <* closePrn

path :: Parser Text
path = fmap T.pack $ many $ noneOf [')', ' ']


imageref :: Parser ImageRef
imageref = imagerefWith (lookAhead parend)

imagerefWith :: Parser Inline -> Parser ImageRef
imagerefWith end = do
  ws3x
  id <- idp <* lexeme (char ':')
  pth <- path <* optWs <* optTitle <* end
  pos <- posFromSource (T.length pth + T.length id + 3) <$> getSourcePos
  return $ ImageRef id pos pth


-- inline-level footnotes and auxiliaries, especially two sentence-level parsers
-- NB: inline-level footnotes are footnotes appearing on words before end-of-sentence punctuation

sentFtn :: Parser SentFtn
sentFtn = SentFtn 0 <$> smallftn <?> "inline-level-footnote"

smallftn :: Parser [Sentence]
smallftn = (lmarker *> ftnParaParts <* rmarker) <?> "smallftn"
  where
    lmarker = string "^["
    rmarker = string "]" <* optional (try newlnStrict)
    ftnParaParts = manyTill (try sentenceInFtn <|> inlines) $ lookAhead rmarker

--NB: ParaPart values constructed with Inlines are disallowed in small footnotes.

--We write two sentence parsers, one for sentences in short footnotes, and another
--for the other environments. Sentences in footnotes cannot end with a right bracket ']'
--since that is reserved for the footnote closing marker.


sentenceInFtn :: Parser Sentence
sentenceInFtn = sentenceWith eosPunct'


sentenceWith' :: Parser Punctuation -> Parser Sentence
sentenceWith' endOfSentencePunct =(fmap Sentence $
  (noMarker *> decoratedInlines <+ endOfSentencePunct <+ optSpace))
  <?> "sentence"



sentenceWith :: Parser Punctuation -> Parser Sentence
sentenceWith endOfSentencePunct =(fmap Sentence $
  (noMarker *> decoratedInlines <+ endOfSentencePunct <+ optSpace) <> afterPunct)
  <?> "sentence"
  where
    afterPunct = try justOneNewline <|> return []
    justOneNewline = notFollowedBy marker *> many newlnStrict

