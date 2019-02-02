{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.POS where

import qualified Data.Text as T

import Control.Applicative hiding ((<|>),many,some)
import qualified Data.Map.Strict as M
import Control.Monad.Trans (liftIO)

import qualified NLP.POS as C

import Text.Megaparsec
import Text.Megaparsec.Char


import Abac.Types.ParserTypes
import Abac.Internal (posFromSource,isCit,isTech,isEmail)
import Abac.Traverse (unwrapParaPart')


import Abac.Parser.Operations
import Abac.Parser.Document
import Abac.Parser.Sections
import Abac.Parser.Blocks
import Abac.Parser.ParaParts
import Abac.Parser.Inlines
import Abac.Parser.Tech


{- NLP
   ---

 Strategy: go over the AST and insert the NLP tags at sentence level. That is,

 1. go down the AST, and for each node select each of its ParaParts in turn, and
 2. extract the Sentences, Inlines etc, and take the list of inlines within them
 3. unwrap these lists to get text stings with a minor modification, namely that
 citations should be rendered as the string "Author", math and code as "TechString".
 4. tag the unwrapped sentence string,
 5. collect each inline and its corresponding tag into a list of tuples
 6. now search the list of tuples for each inline in the original list, and
 update its attribute with the tag

 Formatting notes:
 - possessives become two words: thinker/NN 's/POS
 - erase space before punctuation: mind/NN ,/,  blah/NN ./.
-}

-- print the AST, run the tagger on the resulting text, extract tags and put them back in the AST

documentWithTags :: Parser Document
documentWithTags = do
  Doc secs <- doc
  Doc <$> sequence (fmap addSectionTags secs)

sectionsWithTags :: Parser [Section]
sectionsWithTags = do
  secs <- divisions
  sequence $ fmap addSectionTags secs


addSectionTags :: Section -> Parser Section
addSectionTags (SecBlocks blcs) = fmap SecBlocks $ sequence $ fmap addBlockTags blcs
addSectionTags (Meta mp) = Meta <$> sequence (M.map updateTags mp)
  where
    updateTags :: MetaValue -> Parser MetaValue
    updateTags mv =
      case mv of
        MetaBlocks blcs  -> MetaBlocks <$> sequence (fmap addBlockTags blcs)
        MetaSeq inlss    -> MetaSeq <$> sequence (fmap tagInlines inlss)
        MetaInlines inls -> MetaInlines <$> tagInlines inls
        _                -> return mv
addSectionTags sec@(Section _ _ _ _ _) = addSubsecTags =<< addSecBodyTags sec
  where
    addSecBodyTags :: Section -> Parser Section
    addSecBodyTags sec'@(Meta _) = return sec'
    addSecBodyTags sec'@(SecBlocks _) = return sec'
    addSecBodyTags (Section n l t bdy ss) =
      let wrapBlocksInSection = fmap $ \blcs' -> Section n l t (SecBlocks blcs') ss
          bodyTagUpdates bdy' = sequence (addBlockTags <$> secblcs bdy')
      in  wrapBlocksInSection $ bodyTagUpdates bdy

    addSubsecTags :: Section -> Parser Section
    addSubsecTags sec'@(Meta _) = return sec'
    addSubsecTags sec'@(SecBlocks _) = return sec'
    addSubsecTags sec'@(Section _ _ _ _ []) = return sec'
    addSubsecTags (Section n l t bdy ss) =
      let bodyTagUpdates :: [Section] -> Parser [Section]
          bodyTagUpdates scs = sequence $ fmap addSecBodyTags scs
          subsTagUpdates :: Parser [Section] -> Parser [Section]
          subsTagUpdates pscs = pscs >>= \scs -> sequence $ fmap addSubsecTags scs
          wrapSubsInSection = fmap $ \subs' -> Section n l t bdy subs'
      in  wrapSubsInSection $ subsTagUpdates . bodyTagUpdates $ ss



blockWithTags :: Parser Block
blockWithTags = do
  blc <- block
  addBlockTags blc

addBlockTags :: Block -> Parser Block
addBlockTags blc =
  case blc of
    Para prts          -> Para <$> sequence (addParaPartTags <$> prts)
    Footnote n blcs    -> Footnote n <$> sequence (addBlockTags <$> blcs)
    BlockEx exs        -> BlockEx <$> sequence (addExampleTags <$> exs)
    BlockQuote par     -> BlockQuote <$> addBlockTags par
    BlockComment blcs  -> BlockComment <$> sequence (addBlockTags <$> blcs)
    _                  -> return blc

addExampleTags :: Example -> Parser Example
addExampleTags exmp = addSubexTags =<< addExBodyTags exmp
  where
    addExBodyTags :: Example -> Parser Example
    addExBodyTags (Example o l no nm bdy exs) =
      let wrapBodyInExample = fmap $ \bdy' -> Example o l no nm bdy' exs
          bodyTagUpdate bdy' = fmap ExBody $ sequence $ addParaPartTags <$> exbdy bdy'
      in  wrapBodyInExample $ bodyTagUpdate bdy

    addSubexTags :: Example -> Parser Example
    addSubexTags ex@(Example _ _ _ _ _ []) = addExBodyTags ex
    addSubexTags ex@(Example o l no nm b _) =
      let bodyTagUpdates :: [Example] -> Parser [Example]
          bodyTagUpdates exs' = sequence (fmap addExBodyTags exs')
          subsTagUpdates :: Parser [Example] -> Parser [Example]
          subsTagUpdates pexs = pexs >>= \exs' -> sequence (fmap addSubexTags exs')
          wrapSubsInExample = fmap (Example o l no nm b) :: Parser [Example] -> Parser Example
          updatedBodiesAndSubexs :: Example -> Parser Example
          updatedBodiesAndSubexs (Example _ _ _ _ _ exs') =
            wrapSubsInExample $ subsTagUpdates . bodyTagUpdates $ exs'
      in  updatedBodiesAndSubexs ex


paraPartWithTags :: Parser ParaPart
paraPartWithTags = do
  parapart <- someParaPart
  addParaPartTags parapart

addParaPartTags :: ParaPart -> Parser ParaPart
addParaPartTags parapart = do
  case parapart of
    Sentence _ -> Sentence <$> tagInlinesIn parapart
    Inlines _  -> Inlines <$> tagInlinesIn parapart
    ParaFtn n snts -> fmap (ParaFtn n) $ sequence $ fmap Sentence . tagInlinesIn <$> snts
    ParaComment prts -> fmap ParaComment $ zipWith ($) (fmap theConstructor prts) <$>
      (sequence $ fmap tagInlinesIn prts)
    Caption prts -> fmap Caption $ zipWith ($) (fmap theConstructor prts) <$>
      (sequence $ fmap tagInlinesIn prts)
    _ -> return NullPart
  where
    theConstructor :: ParaPart -> ([Inline] -> ParaPart)
    theConstructor prt =
      case prt of
        Sentence _ -> Sentence
        Inlines _  -> Inlines
        _          -> error "theConstructor function only deals with constructors that wrap inlines."

tagInlinesIn :: ParaPart -> Parser [Inline]
tagInlinesIn parapart = do
  let inls = unwrapParaPart' parapart
  let text = printInlines inls
  tagger <- liftIO C.defaultTagger
  let taggedStr = C.tagStr tagger $ T.unpack text
  let nulltag _ = [(Null, Tag "NOTAG")]
  parseResult <- liftIO $ runParserT sentInlineTagTups "" taggedStr
  let inlineTagTups' = either nulltag id parseResult
  return $ assignTags inls inlineTagTups'

tagInlines :: [Inline] -> Parser [Inline]
tagInlines inls = do
  let text = printInlines inls
  tagger <- liftIO C.defaultTagger
  let taggedStr = C.tagStr tagger $ T.unpack text
  let nulltag _ = [(Null, Tag "NOTAG")]
  let sentOrInlines = try sentInlineTagTups <|> inlineTagTups
  parseResult <- liftIO $ runParserT sentOrInlines "" taggedStr
  let inlineTagTups' = either nulltag id parseResult
  return $ assignTags inls inlineTagTups'

-- some tags are assigned twice, and this may be useful information
-- (e.g. in *foo *bar* foo* the Emph attribute may occur twice on the
-- "bar"-word signifying that it's italicized twice) but here delInline
-- ensures that attributes occur only once
assignTags :: [Inline] -> [(Inline, Tag)] -> [Inline]
assignTags [] _ = []
assignTags (inl:inls) pairs =
  update inl : assignTags inls (delInline inl pairs)
  where
    update :: Inline -> Inline
    update i = updateInlineTag (findTag nulltag i pairs)
             . embeddedAssignment $ i

    -- call assignTags recursively on inlines embedded in Inline values
    embeddedAssignment :: Inline -> Inline
    embeddedAssignment inl' =
      case inl' of
        InlineComment inls'  -> InlineComment (assignTags inls' pairs)
        Link inls' url'      -> Link (assignTags inls' pairs) url'
        Image inls' pth'     -> Image (assignTags inls' pairs) pth'
        _                    -> inl'

    nulltag = Tag "NOTAG"
    cit = Word [None] (0,0) "CITSTR"
    tech = Word [None] (0,0) "TECHSTR"
    mail = Word [None] (0,0) "MAILSTR"

    findTag :: Tag -> Inline -> [(Inline, Tag)] -> Tag
    findTag def _ [] = def
    findTag def inl' ((i,t):tups)
      | isCit inl' && i == cit    = t
      | isTech inl' && i == tech  = t
      | isEmail inl' && i == mail = t
      | inl' == i                 = t
      | otherwise                = findTag def inl' tups

    delInline :: Inline -> [(Inline, Tag)] -> [(Inline, Tag)]
    delInline _ [] = []
    delInline inl' (tup@(i,_):tups)
      | inl' == i  = tups
      | otherwise  = tup : delInline inl' tups

printInlines :: [Inline] -> Text
printInlines inls = T.concat $ printInline <$> inls

printInline :: Inline -> Text
printInline inl =
  case inl of
    Word _ _ txt          -> txt
    Citation _ _ _ _      -> "CITSTR"
    InlineTech _ _ _      -> "TECHSTR"
    Email _ _ _ _         -> "MAILSTR"
    InlineComment inls    -> printInlines inls
    Punct EndSentence txt -> txt `T.append` ws
    Punct _ txt           -> txt
    Number _ _ dbl        -> T.pack $ show dbl
    Link inls url'        -> printInlines inls `T.append` ws `T.append` url'
    Image inls pth        -> printInlines inls `T.append` ws `T.append` pth
    Space                 -> ws
    Newline               -> nl
    ParEnd                -> pe
    _                     -> T.empty
    where
      ws = T.pack " "
      nl = T.pack "\n"
      pe = T.pack "\n\n"



-- inlines with tags


sentInlineTagTups :: Parser [(Inline,Tag)]
sentInlineTagTups = inlineTagTups <+ eosPunctWithTag
  where
    eosPunctWithTag = do
      pct <- eosPunct
      tg <- nlpTag
      return (pct,tg)

inlineTagTups :: Parser [(Inline,Tag)]
inlineTagTups = many (try inlineTagPair <|> citationTagPair)



updateInlineTag :: Tag -> Inline -> Inline
updateInlineTag tg (Word attrs pos txt) = Word (tg:attrs) pos txt
updateInlineTag tg (Citation attrs n pos txt) = Citation (tg:attrs) n pos txt
updateInlineTag tg (Number attrs pos n) = Number (tg:attrs) pos n
updateInlineTag _ inl = inl

inlineWithTag :: Parser Inline
inlineWithTag = do
  inl <- inline
  maybeTg <- optional nlpTag
  case maybeTg of
    Just tg -> return $ updateInlineTag tg inl
    Nothing -> return inl

inlineTagPair :: Parser (Inline, Tag)
inlineTagPair = do
  inl <- inline
  maybeTg <- optional nlpTag
  case maybeTg of
    Just tg -> return (inl, tg)
    Nothing -> return (inl, Tag "NOTAG")

citationWithTag :: Parser Inline
citationWithTag = do
  char '@'
  pos <- posFromSource 1 <$> getSourcePos
  nlpTag
  char ' '
  citKey <- some citKeyChar
  tg <- nlpTag
  return $ Citation [tg] 0 pos (T.pack citKey)
  where
    citKeyChar = alphaNumChar <|> oneOf ("-:" :: String)

citationTagPair :: Parser (Inline,Tag)
citationTagPair = do
  char '@'
  pos <- posFromSource 1 <$> getSourcePos
  nlpTag
  char ' '
  citKey <- some citKeyChar
  tg <- nlpTag
  return (Citation [None] 0 pos (T.pack citKey), tg)
  where
    citKeyChar = alphaNumChar <|> oneOf ("-:" :: String)



mathInlineNoTag :: Parser Inline
mathInlineNoTag = do
  InlineTech tech pos strWithTags <- mathInline
  let skipTags = many (satisfy (\c -> c /= '/') <* nlpTag)
  let putError _ = "math string at position " ++ show pos
  let ioStrNoTags = T.pack . either putError id <$> runParserT skipTags "" (T.unpack strWithTags)
  strWithoutTags <- liftIO ioStrNoTags
  return $ InlineTech tech pos strWithoutTags

nlpTag :: Parser Tag
nlpTag = frwSlash *> tag <* optSpaceBeforePunct
  where
    frwSlash = char '/'
    tag = fmap (Tag . T.pack) $ try (some upperChar) <|> fmap pure tagChar
    optSpaceBeforePunct = optional $ try $ some (char ' ') *> lookAhead punctuationChar
    tagChar = satisfy $ \c ->
      c == '.' ||
      c == ',' ||
      c == ':' ||
      c == '@'

