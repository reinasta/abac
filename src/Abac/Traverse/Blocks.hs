{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Blocks where

import qualified Data.Map.Strict as M


import Abac.Types.ParserTypes
import Abac.Internal

import Abac.Traverse.Internal
import Abac.Traverse.Predicates
import Abac.Traverse.ParaParts

--get list items from Blocks

blocksToSentences :: [Block] -> [[Sentence]]
blocksToSentences blcs = fmap blockToSentences blcs

blockToSentences :: Block -> [Sentence]
blockToSentences blc = paraPartsToSents $ blockToParaParts blc

blockToInlines :: Block -> [Inline]
blockToInlines blc = concatMap unwrapParaPart' $ blockToParaParts blc

getWordLikesFromBlock :: Block -> [Inline]
getWordLikesFromBlock blc = filter wordlike $ blockToInlines blc

hasMoreThan :: Block -> Int -> Bool
hasMoreThan blc int = (length $ getWordLikesFromBlock blc) >= int

hasLessThan :: Block -> Int -> Bool
hasLessThan blc int = not $ hasMoreThan blc int

-- hasSentsWithMoreThan :: Block -> Int -> Bool
-- hasSentsWithMoreThan blc int =
--   let sentsInBlock = blockToSentences blc
--   in any overTheLimit sentsInBlock

overTheLimit :: Int -> Sentence -> Bool
overTheLimit int snt = (length $ getWordLikesFromSentence snt) >= int

underTheLimit :: Int -> Sentence -> Bool
underTheLimit int snt = (length $ getWordLikesFromSentence snt) < int

-- hasSentsWithLessThan :: Block -> Int -> Bool
-- hasSentsWithLessThan blc int = not $ hasSentsWithMoreThan blc int

getWordLikesFromSentence :: Sentence -> [Inline]
getWordLikesFromSentence = unwrapParaPart'

getLongSents :: Int -> Block -> [Sentence]
getLongSents int blc = filter (overTheLimit int) (blockToSentences blc)

getShortSents :: Int -> Block -> [Sentence]
getShortSents int blc = filter (underTheLimit int) (blockToSentences blc)


-- get blocks

getBlocks :: Document -> [Block]
getBlocks doc = divsToBlocks $ getSections doc

divsToBlocks :: [Section] -> [Block]
divsToBlocks [] = []
divsToBlocks (sec:secs) = unwrapSection' sec ++ divsToBlocks secs

unwrapSection' :: Section -> [Block]
unwrapSection' (SecBlocks blcs) = blcs
unwrapSection' (Section _ _ _ bdy secs) = (secblcs . secbdy $ bdy) ++ concatMap unwrapSection' secs
unwrapSection' (Meta mp) = let MetaBlocks blcs = getAbstract' mp in blcs
  where getAbstract' = M.findWithDefault (MetaBlocks []) AbstractKey


