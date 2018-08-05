{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Sections where

import Control.Error.Util (note)
import qualified Data.Map.Strict as M
import qualified Data.Text as T (Text,pack,unpack,splitOn)
import Prelude hiding (Word)

import Abac.Types.ParserTypes
import Abac.Internal
import Abac.Traverse.Internal
import Abac.Traverse.Predicates



--search sections

searchSection :: T.Text -> Document -> Either T.Text Section
searchSection txt doc =
  case txt of
    "meta"    -> getMeta doc
    "intro"   -> getIntro doc
    otherText -> getSection (mkNo otherText) doc

mkNo :: T.Text -> No
mkNo txt = listToTup $ intList txt ++ take r (repeat 0)
  where
    r = 6 - length (intList txt)
    intList t = read <$> T.unpack <$> T.splitOn ("." :: T.Text) t :: [Int]

mapSections :: [Section] -> M.Map T.Text Section
mapSections [] = M.empty
mapSections (sct:scts)
  | isMeta sct    = M.insert "meta" sct (mapSections scts)
  | isPureDiv sct = M.insert "intro" sct (mapSections scts)
  | otherwise     = M.insert (T.pack $ showSecNo $ secno sct) sct (mapSections scts)

--Get lists of items from Documents

getSections :: Document -> [Section]
getSections (Doc secs) = secs

getTitledSections :: Document -> [Section]
getTitledSections doc = filter isSection $ getSections doc

getAllSections :: Document -> [Section]
getAllSections doc = allSections $ getSections doc

getSection :: No -> Document -> Either T.Text Section
getSection no doc =
  let msg = T.pack $ "couldn't find section numbered " ++ showSecNo no
  in  note msg $ lookSectionUp no $ getAllSections doc

lookSectionUp :: No -> [Section] -> Maybe Section
lookSectionUp _ [] = Nothing
lookSectionUp no (sec:secs)
  | sectionNo sec == no  = Just sec
  | otherwise            = lookSectionUp no secs
  where
    sectionNo sct@(Section _ _ _ _ _) = secno sct
    sectionNo _ = zeros


allSections :: [Section] -> [Section]
allSections [] = []
allSections (sect:sects) = disect sect ++ allSections sects
  where
    disect :: Section -> [Section]
    disect sec =
      case sec of
        s@(Section _ _ _ _ subs) -> s : concatMap disect subs
        _                        -> [sec]

sectionMap' :: [Section] -> M.Map No Section
sectionMap' [] = M.empty
sectionMap' (sec:secs) =
  case sec of
    Section no _ _ _ _ -> M.insert no sec $ sectionMap' secs
    _                  -> sectionMap' secs

sectionMap :: Document -> M.Map No Section
sectionMap doc = sectionMap' $ getAllSections doc


--Get lists of items from Sections

getBlocksFromDiv :: Section -> [Block]
getBlocksFromDiv sec = unwrapSection' sec

getParasFromDiv :: Section -> [Paragraph]
getParasFromDiv sec = filter isParagraph $ unwrapSection' sec

getFootnotesFromDiv :: Section -> [Footnote]
getFootnotesFromDiv sec = getFootnoteBFromDiv sec ++ getFootnoteSFromDiv sec

getFootnoteSFromDiv :: Section -> [Footnote]
getFootnoteSFromDiv sec = parapartsToFootnotesS . getParaPartsFromDiv $ sec

getFootnoteBFromDiv :: Section -> [Footnote]
getFootnoteBFromDiv sec = blocksToFootnotesB . getBlocksFromDiv $ sec

getOrderedListsFromDiv :: Section -> [Example]
getOrderedListsFromDiv sec =
   filter isOrdered $ concatMap unwrapBlockEx $ filter isBlockEx (getBlocksFromDiv sec)

getUnorderedListsFromDiv :: Section -> [Example]
getUnorderedListsFromDiv sec =
   filter isUnordered $ concatMap unwrapBlockEx $ filter isBlockEx (getBlocksFromDiv sec)

getBlockQuotesFromDiv :: Section -> [Quote]
getBlockQuotesFromDiv sec = filter isQuote $ getBlocksFromDiv sec

getBlockTechFromDiv :: Section -> [BlockTech]
getBlockTechFromDiv sec = filter isBlockTech $ getBlocksFromDiv sec

getTablesFromDiv :: Section -> [Table]
getTablesFromDiv sec = filter isTable $ getBlocksFromDiv sec

getParaPartsFromDiv :: Section -> [ParaPart]
getParaPartsFromDiv sec = blocksToParaParts . getBlocksFromDiv $ sec

getSentencesFromDiv :: Section -> [Sentence]
getSentencesFromDiv sec = paraPartsToSents . getParaPartsFromDiv $ sec

getLongSentsFromDiv :: Int -> Section -> [Sentence]
getLongSentsFromDiv int sec = filter (overTheLimit int) (getSentencesFromDiv sec)

getShortSentsFromDiv :: Int -> Section -> [Sentence]
getShortSentsFromDiv int sec = filter (underTheLimit int) (getSentencesFromDiv sec)


getInlinePartsFromDiv :: Section -> [ParaPart]
getInlinePartsFromDiv sec = paraPartsToInlineParts . getParaPartsFromDiv $ sec

getInlinesFromDiv :: Section -> [Inline]
getInlinesFromDiv sec = concatMap unwrapParaPart' $ getParaPartsFromDiv sec

getCitationsFromDiv :: Section -> [Citation]
getCitationsFromDiv sec = filter isCit $ getInlinesFromDiv sec

getLinkFromDiv :: Section -> [Link]
getLinkFromDiv sec = filter isLink $ getInlinesFromDiv sec

getImageFromDiv :: Section -> [Image]
getImageFromDiv sec = filter isImage $ getInlinesFromDiv sec

getWordsFromDiv :: Section -> [Word]
getWordsFromDiv sec = filter isWord $ getInlinesFromDiv sec

getWordLikesFromDiv :: Section -> [Word]
getWordLikesFromDiv sec = filter wordlike $ getInlinesFromDiv sec

