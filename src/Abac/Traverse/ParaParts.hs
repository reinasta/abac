module Abac.Traverse.ParaParts where

import Abac.Types.ParserTypes
import Abac.Internal

import Abac.Traverse.Internal
import Abac.Traverse.Searchable
import Abac.Traverse.Predicates


getQuotes :: Document -> [Quote]
getQuotes doc = filter isQuote $ getBlocks doc

getParaParts :: Document -> [ParaPart]
getParaParts doc = blocksToParaParts $ getBlocks doc

blocksToParaParts :: [Block] -> [ParaPart]
blocksToParaParts [] = []
blocksToParaParts (b:bs) = blockToParaParts b ++ blocksToParaParts bs

blocksToParaParts' :: [Block] -> [[ParaPart]]
blocksToParaParts' [] = []
blocksToParaParts' (b:bs) = blockToParaParts b : blocksToParaParts' bs

blockToParaParts :: Block -> [ParaPart]
blockToParaParts b =
  case b of
    Para prts          -> prts
    Table prts         -> prts
    BlockQuotes blcs   -> concatMap blockToParaParts blcs
    BlockComment blcs  -> concatMap blockToParaParts blcs
    Footnote _ blcs    -> blocksToParaParts blcs
    BlockEx exs        -> unwrapExs exs
    _                  -> []
  where
    unwrapExs exs = concatMap unwrapEx exs

    unwrapEx (Example _ _ _ _ bdy []) = exbdy bdy
    unwrapEx (Example _ _ _ _ bdy subexs) = exbdy bdy ++ unwrapExs subexs

-- skip comment blocks and big (block) comments
blockToParaParts' :: Block -> [ParaPart]
blockToParaParts' b =
  case b of
    Para prts          -> prts
    Table prts         -> prts
    BlockQuotes blcs   -> concatMap blockToParaParts blcs
    BlockEx exs        -> unwrapExs exs
    _                  -> []
  where
    unwrapExs exs = concatMap unwrapEx exs

    unwrapEx (Example _ _ _ _ bdy []) = exbdy bdy
    unwrapEx (Example _ _ _ _ bdy subexs) = exbdy bdy ++ unwrapExs subexs




getSentences :: Document -> [Sentence]
getSentences doc = paraPartsToSents . getParaParts $ doc

paraPartsToSents :: [ParaPart] -> [Sentence]
paraPartsToSents [] = []
paraPartsToSents (p:ps) = case p of
  sent@(Sentence _) -> [sent] ++ paraPartsToSents ps
  (ParaFtn _ snts)  -> snts ++ paraPartsToSents ps
  (Inlines _)       -> paraPartsToSents ps
  (Caption prts)    -> paraPartsToSents prts ++ paraPartsToSents ps
  ParaComment prts  -> paraPartsToSents prts ++ paraPartsToSents ps
  _                 -> paraPartsToSents ps

getFootnotes :: Document -> [Footnote]
getFootnotes doc = blocksToFootnotesB (getBlocks doc) ++ parapartsToFootnotesS (getParaParts doc)

blocksToFootnotesB :: [Block] -> [Footnote]
blocksToFootnotesB [] = []
blocksToFootnotesB (b:bs) = case b of
  ftn@(Footnote _ _) -> FtnB ftn : blocksToFootnotesB bs
  _                  -> blocksToFootnotesB bs

parapartsToFootnotesS :: [ParaPart] -> [Footnote]
parapartsToFootnotesS [] = []
parapartsToFootnotesS (p:ps) = case p of
  ftn@(ParaFtn _ _) -> FtnS ftn : parapartsToFootnotesS ps
  _                 -> parapartsToFootnotesS ps

getInlineParts :: Document -> [Inlines]
getInlineParts doc = paraPartsToInlineParts $ getParaParts doc

paraPartsToInlineParts :: [ParaPart] -> [ParaPart]
paraPartsToInlineParts [] = []
paraPartsToInlineParts (p:ps) = case p of
  i@(Inlines _) -> i : paraPartsToInlineParts ps
  _             -> paraPartsToInlineParts ps


getParagraphs :: Document -> [Paragraph]
getParagraphs doc = filter isParagraph $ getBlocks doc

getExamples :: Document -> [Example]
getExamples doc = filter isOrdered $ concatMap unwrapBlockEx $ filter isBlockEx (getBlocks doc)

--getExamples :: Document -> [[Example]]
--getExamples doc = concatMap unwrapOrdered $ filter isOrdered $ getBlocks doc

unwrapBlockEx :: Block -> [Example]
unwrapBlockEx (BlockEx exs) = exs
unwrapBlockEx _ = []


getItems :: Document -> [Example]
getItems doc = filter isUnordered $ concatMap unwrapBlockEx $ filter isBlockEx (getBlocks doc)



