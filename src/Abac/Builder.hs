{-# LANGUAGE OverloadedStrings #-}
module Abac.Builder where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Map.Strict as M (fromList)
import Prelude hiding (Word)

import Abac.Types.ParserTypes
import Abac.Internal (tupToList,listToTup,embedAllSections,mkWords)

-- mkExpr :: [Inline] -> Expression
-- mkExpr inls = Expression inls

mkExprs :: [[Inline]] -> [Expression]
mkExprs inlss = fmap Expression inlss


-- make doc; note the constraints on sections, blocks, and
-- sentences, as defined below
mkDoc :: [(T.Text,T.Text,T.Text)] -> Document
mkDoc tups = Doc $ mkSections tups

-- make a simple yaml section; date format is "1989-09-21"
mkYaml :: T.Text -> [T.Text] -> T.Text -> T.Text -> Meta
mkYaml ttl auths dte abstr =
  Meta $ M.fromList [(TitleKey,ttl'), (AuthorKey,auths'), (DateKey,dte'),(AbstractKey,abs')]
  where
    ttl' = MetaInlines $ mkWords' ttl
    auths' = MetaSeq $ mkWords' <$> auths
    dte' = MetaNum $ read . T.unpack <$> T.splitOn "-" dte
    abs' = MetaBlocks $ mkPars abstr

-- make a simple set of sections out of a list of string-tuples
mkSections :: [(Text,Text,Text)] -> [Section]
mkSections tups = embedAllSections $ sectionsFromTuples <$> tups
  where
    sectionsFromTuples (f,s,t) = mkSection f s t []

-- make a simple section, with paragraphs (and no other blocks),
-- made out of words (and no other inlines)
mkSection :: T.Text -> T.Text -> T.Text -> [Section] -> Section
mkSection no ttl bdy subs =
  let no' = maybe zeros id (stringToNo no)
      lev = length $ takeWhile (/= 0) (tupToList no')
      ttl' = mkTitle ttl
      bdy' = mkSectionBlocks bdy
  in Section no' lev ttl' bdy' subs


-- make words from text
mkWords' :: T.Text -> [Word]
mkWords' txt = mkWords $ T.words $ T.strip txt

-- make title from text; the end-of-sentence punctuation
-- will not be represented in the Title value
mkTitle :: T.Text -> Title
mkTitle txt = Title $ mkWords' txt

-- the Sentence value will not include any end-of-sentence punctuation,
-- which is handled separately in mkPar
mkSent :: T.Text -> Sentence
mkSent txt = Sentence $ mkWords' txt

-- the Paragraph value will not include the newlines that end the paragraph,
-- which is handled separately in mkPars. The paragraph includes only sentences.
mkPar :: T.Text -> Paragraph
mkPar txt = Para $ mkSent <$> textSents txt
  where
    textSents txt' = filter (not . T.null) (T.split isPunct' txt')
    isPunct' c = c == '.' || c == '!' || c == '?'

mkPars :: T.Text -> [Paragraph]
mkPars txt = mkPar <$> textPars txt
  where
    textPars txt' = filter (not . T.null) $ T.splitOn "\n\n" $ T.strip txt'

-- this makes a section without title, build like this: SecBlocks pars
mkSectionBlocks :: T.Text -> Section
mkSectionBlocks txt = SecBlocks $ mkPars txt

-- equivalent to Internal.mkNo
stringToNo :: T.Text -> Maybe No
stringToNo txt = mkNo' <$> maybeInts
  where
    mkNo' ints = listToTup $ ints ++ replicate (6 - length ints) 0
    maybeInts = sequence $ either (\_ -> Nothing) Just <$> eithers :: Maybe [Int]
    splits = T.splitOn "." (T.strip txt) :: [T.Text]
    convertToInt :: [Either String (Integer, T.Text)] -> [Either String Int]
    convertToInt = (fmap . fmap) (fromIntegral . fst)
    eithers = convertToInt $ fmap T.decimal splits :: [Either String Int]

