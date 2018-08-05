module Abac.PartsOfSpeech.Sentences where

import Data.List
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Error.Util (note)
import Prelude hiding (Word)

-- import qualified Options as Opt

import Abac.Types
import Abac.Internal (ParaIndex,mapit,(%>),average)
import Abac.Traverse (showSecNo,getShortSentsFromDiv,getLongSentsFromDiv,
                      sectionMap,getBlocksFromDiv,getBlocks,hasMoreThan,
                      getWordLikesFromBlock,getWordLikesFromSentence,getShortSents,
                      getLongSents)


--Sentences
---------

paraLongAllCustom :: Int -> Document -> Map ParaIndex Block
paraLongAllCustom n doc = M.filter (flip hasMoreThan $ n) (mapit $ getBlocks doc)

sentLongAllCustom :: Int -> Document -> Map ParaIndex [Sentence]
sentLongAllCustom n doc = M.filter (not . null) $ getLongSents n <$> (mapit $ getBlocks doc)

sentShortAllCustom :: Int -> Document -> Map ParaIndex [Sentence]
sentShortAllCustom n doc = M.filter (not . null) $ getShortSents n <$> (mapit $ getBlocks doc)

paraLongAllDef :: Document -> Map ParaIndex Block
paraLongAllDef = paraLongAllCustom $ paralongNo defaultOptions

sentLongAllDef :: Document -> Map ParaIndex [Sentence]
sentLongAllDef = sentLongAllCustom $ sentlongNo defaultOptions

sentShortAllDef :: Document -> Map ParaIndex [Sentence]
sentShortAllDef = sentShortAllCustom $ sentshortNo defaultOptions


paraLongAtCustom :: Int -> ParaIndex -> Document -> Either T.Text Block
paraLongAtCustom n i doc = note msg $ M.lookup i $ paraLongAllCustom n doc
  where msg = T.pack "By golly, methinks you are so stylish!"

sentLongAtCustom :: Int -> ParaIndex -> Document -> Either T.Text [Sentence]
sentLongAtCustom n i doc = note msg $ M.lookup i $ sentLongAllCustom n doc
  where msg = T.pack "By golly, methinks you are so stylish!"

sentShortAtCustom :: Int -> ParaIndex -> Document -> Either T.Text [Sentence]
sentShortAtCustom n i doc =  note msg $ M.lookup i $ sentShortAllCustom n doc
  where msg = T.pack "By golly, methinks you are so stylish!"

paraLongAtDef :: ParaIndex -> Document -> Either T.Text Block
paraLongAtDef = paraLongAtCustom (paralongNo defaultOptions)

sentLongAtDef :: ParaIndex -> Document -> Either T.Text [Sentence]
sentLongAtDef = sentShortAtCustom (sentlongNo defaultOptions)

sentShortAtDef :: ParaIndex -> Document -> Either T.Text [Sentence]
sentShortAtDef = sentShortAtCustom (sentshortNo defaultOptions)

sentLongAtCount :: Int -> ParaIndex -> Document -> Either T.Text Int
sentLongAtCount n i doc = length <$> sentLongAtCustom n i doc

sentLongAtPercent :: Int -> ParaIndex -> Document -> Either T.Text Double
sentLongAtPercent n i doc = (%>) <$> numerator <*> denominator
  where
    numerator = fromIntegral . sum <$> fmap (length . getWordLikesFromSentence) <$>
      sentLongAtCustom n i doc
    denominator = fmap fromIntegral $ length . getWordLikesFromBlock <$>
      (note msg $ M.lookup i $ mapit $ getBlocks doc)
    msg = T.pack $ "Couldn't find block " ++ show i

sentLongAllPercent :: Document -> Either T.Text Double
sentLongAllPercent doc =
  Right $ sentsWithLength (sentlongNo defaultOptions) %> sentsWithLength 0
  where
    sentsWithLength n = fromIntegral . sum $ fmap (length . getWordLikesFromSentence) $
      concat . M.elems $ sentLongAllCustom n doc


sentShortAtCount :: Int -> ParaIndex -> Document -> Either T.Text Int
sentShortAtCount n i doc = length <$> sentShortAtCustom n i doc

sentShortAtPercent :: Int -> ParaIndex -> Document -> Either T.Text Double
sentShortAtPercent n i doc = (%>) <$> numerator <*> denominator
  where
    numerator = fromIntegral . sum <$> fmap (length . getWordLikesFromSentence) <$>
      sentShortAtCustom n i doc
    denominator = fmap fromIntegral $ length . getWordLikesFromBlock <$>
      (note msg $ M.lookup i $ mapit $ getBlocks doc)
    msg = T.pack $ "Couldn't find block " ++ show i

sentShortAllPercent :: Document -> Either T.Text Double
sentShortAllPercent doc = Right $
  lengthOf (sentShortAllCustom (sentshortNo defaultOptions) doc) %>
  lengthOf (sentLongAllCustom 0 doc)
  where
    lengthOf sents = fromIntegral . sum $ fmap (length . getWordLikesFromSentence) $
      concat . M.elems $ sents -- sentLongAllCustom n doc

paraLongAtCount :: Int -> ParaIndex -> Document -> Either T.Text Int
paraLongAtCount n i doc = fmap (length . getWordLikesFromBlock) $
  note msg $ M.lookup i $ paraLongAllCustom n doc
  where msg = T.pack "By golly, methinks you are so stylish!"

paraLongAtPercent :: Int -> ParaIndex -> Document -> Either T.Text Double
paraLongAtPercent n i doc = (%>) <$> targetParaLength doc <*> avgParaLength doc
  where
    targetParaLength dc = fmap fromIntegral $ paraLongAtCount n i dc
    avgParaLength dc = Right $ average $ paraLengths dc
    paraLengths dc = fmap (fromIntegral . length . getWordLikesFromBlock) (getBlocks dc)

--section numbers

paraLongAtSectionAll :: Document -> Map No [Block]
paraLongAtSectionAll doc = filter (flip hasMoreThan $ paralongNo defaultOptions) <$>
  fmap getBlocksFromDiv (sectionMap doc)

paraLongAtSection :: No -> Document -> Either T.Text [Block]
paraLongAtSection no doc = note msg $ M.lookup no (paraLongAtSectionAll doc)
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

paraLongCountAtSectionAll :: Document -> Map No Int
paraLongCountAtSectionAll doc = length <$> paraLongAtSectionAll doc

paraLongCountAtSection :: No -> Document -> Either T.Text Int
paraLongCountAtSection no doc = note msg $ M.lookup no $ paraLongCountAtSectionAll doc
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

sentLongAtSectionAll :: Document -> Map No [Sentence]
sentLongAtSectionAll doc = M.filter (not . null) $ getLongSentsFromDiv (sentlongNo defaultOptions) <$>
  (sectionMap doc)

sentLongAtSection :: No -> Document -> Either T.Text [Sentence]
sentLongAtSection no doc = note msg $ M.lookup no (sentLongAtSectionAll doc)
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

sentLongCountAtSectionAll :: Document -> Map No Int
sentLongCountAtSectionAll doc = length <$> sentLongAtSectionAll doc

sentLongCountAtSection :: No -> Document -> Either T.Text Int
sentLongCountAtSection no doc = note msg $ M.lookup no $ sentLongCountAtSectionAll doc
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

sentShortAtSectionAll :: Document -> Map No [Sentence]
sentShortAtSectionAll doc = M.filter (not . null) $
  getShortSentsFromDiv (sentlongNo defaultOptions) <$>
  (sectionMap doc)

sentShortAtSection :: No -> Document -> Either T.Text [Sentence]
sentShortAtSection no doc = note msg $ M.lookup no (sentShortAtSectionAll doc)
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

sentShortCountAtSectionAll :: Document -> Map No Int
sentShortCountAtSectionAll doc = length <$> sentShortAtSectionAll doc

sentShortCountAtSection :: No -> Document -> Either T.Text Int
sentShortCountAtSection no doc = note msg $ M.lookup no $ sentShortCountAtSectionAll doc
  where
    msg = T.pack $ "I cannot find section " ++ showSecNo no

