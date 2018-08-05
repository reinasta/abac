module Abac.PartsOfSpeech.Counters where

import Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Error.Util (note)
import Data.Function (on)
import Prelude hiding (Word)

-- import qualified Options as Opt

import Abac.Types
import Abac.Internal (ParaIndex,mapit,wordlike)
import Abac.Traverse (showSecNo,getWordLikesFromDiv,wordsAtLines,taggedLookup,
                     getInlines,getBlocks,selectInlinesWithNeg,sectionMap,getInlinesFromDiv,
                     getSection,getWordLikesFromBlock,selectBlockWithNeg)



--Counter
---------

stopwords :: [Word]
stopwords = stopwordKW defaultKeywords

--block-level counters

countWordsAtCustom :: ParaIndex
                   -> Document
                   -> Either T.Text Int
countWordsAtCustom i doc = note msg $ M.lookup i $
  (length . getWordLikesFromBlock) <$> (mapit $ getBlocks doc)
  where msg = T.pack "By golly, methinks you are so stylish!"

countUncommonAtCustom :: Int
                      -> ParaIndex
                      -> Document
                      -> Either T.Text Int
countUncommonAtCustom n i doc = note msg $ M.lookup i $ M.filter (>= n) $
  (length . selectBlockWithNeg (==) stopwords) <$> (mapit $ getBlocks doc)
  where msg = T.pack "By golly, methinks you are so stylish!"

countUncommonAtCustom' :: Int
                       -> ParaIndex
                       -> Document
                       -> Either T.Text [(Word,Int)]
countUncommonAtCustom' n i doc =
  note msg $
    M.lookup i $
      moreThan n $
        mkUncommonPairs <$>
          blocksMap
  where
    moreThan :: Int
             -> Map ParaIndex [(Word,Int)]
             -> Map ParaIndex [(Word,Int)]
    moreThan n' mp = M.filter ((>=) n' . length) mp
    mkUncommonPairs :: Block -> [(Word,Int)]
    mkUncommonPairs = countThem . filter wordlike .
      selectBlockWithNeg (/=) stopwords
    blocksMap = mapit $ getBlocks doc :: Map ParaIndex Block
    msg = T.pack "By golly, methinks you are so stylish!"

countUncommonAtDef :: ParaIndex
                   -> Document
                   -> Either T.Text Int
countUncommonAtDef i doc = countUncommonAtCustom (countspecNo defaultOptions) i doc


--line-level counters

countWordsAllLines :: Document -> Map LineNo Int
countWordsAllLines doc = length <$> wordsAtLines doc

countWordsAtLines :: LineNo -> Document -> Maybe Int
countWordsAtLines i doc = M.lookup i $ countWordsAllLines doc

countUncommonAllLines :: Document -> Map LineNo Int
countUncommonAllLines doc =
  length <$> selectInlinesWithNeg (==) stopwords <$> (wordsAtLines doc)

countUncommonAtLines :: LineNo -> Document -> Maybe Int
countUncommonAtLines i doc = M.lookup i $ countUncommonAllLines doc

--section-level counters

countUncommonInSection :: No -> Document -> Either T.Text Int
countUncommonInSection no doc =
  let countWordsIn' = fmap (length . onlyUncommon . filter wordlike)
      inlines = getInlinesFromDiv <$> getSection no doc
      onlyUncommon = selectInlinesWithNeg (/=) stopwords
  in  countWordsIn' inlines

countWordsInSection :: No -> Document -> Either T.Text Int
countWordsInSection no doc =
  let countWordsIn' = fmap (length . filter wordlike)
      inlines = getInlinesFromDiv <$> getSection no doc
  in  countWordsIn' inlines


--document-level counters

wordsUncommon :: Document -> [Word]
wordsUncommon doc = selectInlinesWithNeg (/=) stopwords $
  filter wordlike (getInlines doc)

countThem :: [Word] -> [(Word,Int)]
countThem wrds = [ (head ws,length ws) | ws <- group $ sort wrds ]

countWordsUncommon :: Document -> [(Word,Int)]
countWordsUncommon = reverse . sortBy (compare `on` snd) . countThem . wordsUncommon

wordCountAllCustom :: Int -> Document -> Map ParaIndex Int
wordCountAllCustom n doc = moreThan n $ (length . getWordLikesFromBlock) <$> (mapit $ getBlocks doc)
  where moreThan n' mp = M.filter (>= n') mp

wordCountAtCustom :: Int -> ParaIndex -> Document -> Either Text Int
wordCountAtCustom n i doc = taggedLookup i $ wordCountAllCustom n doc

wordCountAllDef :: Document -> Map ParaIndex Int
wordCountAllDef doc = wordCountAllCustom 1 doc

wordCountAtDef :: ParaIndex -> Document -> Either Text Int
wordCountAtDef i doc = taggedLookup i $ wordCountAllDef doc

wordCount :: Document -> Either a Int
wordCount doc = Right $ length $ filter wordlike (getInlines doc)

--line functions

uncommonCountAllLine :: Document -> Map LineNo Int
uncommonCountAllLine doc =
  let uncommon = selectInlinesWithNeg (/=) stopwords
  in  (length . uncommon) <$> wordsAtLines doc

uncommonCountAtLine :: LineNo -> Document -> Either Text Int
uncommonCountAtLine i doc = taggedLookup i $ uncommonCountAllLine doc

wordCountAllLine :: Document -> Map LineNo Int
wordCountAllLine doc = length <$> wordsAtLines doc

wordCountAtLine :: LineNo -> Document -> Either Text Int
wordCountAtLine i doc = taggedLookup i $ wordCountAllLine doc

--section numbers


wordAtSectionAll :: Document -> Map No [Word]
wordAtSectionAll doc = fmap getWordLikesFromDiv
  (sectionMap doc)

wordAtSection :: No -> Document -> Either T.Text [Word]
wordAtSection no doc = note msg $ M.lookup no (wordAtSectionAll doc)
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

wordCountAtSectionAll :: Document -> Map No Int
wordCountAtSectionAll doc = length <$> wordAtSectionAll doc

wordCountAtSection :: No -> Document -> Either T.Text Int
wordCountAtSection no doc = note msg $ M.lookup no (wordCountAtSectionAll doc)
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

