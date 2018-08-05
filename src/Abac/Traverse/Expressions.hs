{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Expressions where

import qualified Data.Map.Strict as M
import Prelude hiding (Word)

import Abac.Types.ParserTypes
--import Abac.Internal

import Abac.Traverse.Internal
import Abac.Traverse.Textify


--formerly expressionWith

expressionsWithInExprs :: Int -> [Word] -> [Expression] -> [Expression]
expressionsWithInExprs _ _ [] = []
expressionsWithInExprs _ [] _ = []
expressionsWithInExprs n ws (e : es)
  | countElemsOfInExpr ws e >= n = e : expressionsWithInExprs n ws es
  | otherwise                    = expressionsWithInExprs n ws es

nForNgrams :: Int
nForNgrams = 7

--ngramsInInlines' :: Int -> [Inline] -> [Expression]

expressionsWithInSentence :: Int -> [Word] -> Sentence -> [Expression]
expressionsWithInSentence n refs (Sentence inls) =
  expressionsWithInExprs n refs $ ngramsInInlines' nForNgrams inls
expressionsWithInSentence _ _ _ = []

expressionsWithInSentences :: Int -> [Word] -> [Sentence] -> [Expression]
expressionsWithInSentences n refs snts = concatMap (expressionsWithInSentence n refs) snts

expressionsWithInParaPart :: Int -> [Word] -> Sentence -> [Expression]
expressionsWithInParaPart n refs sent@(Sentence _) = expressionsWithInSentence n refs sent
expressionsWithInParaPart n refs (ParaFtn _ snts) = expressionsWithInSentences n refs snts
expressionsWithInParaPart n refs (Inlines inls) =
  expressionsWithInExprs n refs (ngramsInInlines' nForNgrams inls)
expressionsWithInParaPart n refs (Caption prts) = expressionsWithInParaParts n refs prts
expressionsWithInParaPart _ _ _ = []

expressionsWithInParaParts :: Int -> [Word] -> [ParaPart] -> [Expression]
expressionsWithInParaParts n refs prts = concatMap (expressionsWithInParaPart n refs) prts

-- expressionsWithInExample :: Int -> [Word] -> Example -> [Expression]
-- expressionsWithInExample n refs (MainEx _ _ _ prts) = expressionsWithInParaParts n refs prts
-- expressionsWithInExample n refs (SubEx _ _ _ prts) = expressionsWithInParaParts n refs prts

expressionsWithInBlock :: Int -> [Word] -> Block -> [Expression]
expressionsWithInBlock n refs (Para prts) = expressionsWithInParaParts n refs prts
expressionsWithInBlock n refs (Footnote _ blcs) = expressionsWithInBlocks n refs blcs
expressionsWithInBlock n refs (BlockEx exs) = expressionsWithInParaParts n refs (unwrapExamples exs)
expressionsWithInBlock n refs (BlockQuotes blcs) = expressionsWithInBlocks n refs blcs
expressionsWithInBlock n refs (BlockQuote par) = expressionsWithInBlock n refs par
expressionsWithInBlock _ _ _ = []

expressionsWithInBlocks :: Int -> [Word] -> [Block] -> [Expression]
expressionsWithInBlocks n refs blcs = concatMap (expressionsWithInBlock n refs) blcs

expressionsWithInSection :: Int -> [Word] -> Section -> [Expression]
expressionsWithInSection n refs (SecBlocks blcs) = expressionsWithInBlocks n refs blcs
expressionsWithInSection n refs (Section _ _ (Title inls) bdy subs) =
  expressionsWithInExprs n refs (ngramsInInlines' nForNgrams inls) ++
  expressionsWithInBlocks n refs (secblcs . secbdy $ bdy) ++
  expressionsWithInSections n refs subs
expressionsWithInSection n refs (Meta mp) = authorExprs ++ titleExprs ++ abstractExprs ++ tagExprs
  where
    authorExprs = procAuthor mp
      (concatMap (expressionsWithInExprs n refs . ngramsInInlines' nForNgrams)) []
    titleExprs = procTitle mp
      (expressionsWithInExprs n refs . ngramsInInlines' nForNgrams) []
    abstractExprs = procAbstract mp (expressionsWithInBlocks n refs) []
    tagExprs = procTags mp (expressionsWithInExprs n refs . ngramsInInlines' nForNgrams) []

expressionsWithInSections :: Int -> [Word] -> [Section] -> [Expression]
expressionsWithInSections _ _ [] = []
expressionsWithInSections n refs (sec:secs) =
  expressionsWithInSection n refs sec ++ expressionsWithInSections n refs secs


expressionsWithInDocument :: Int -> [Word] -> Document -> [Expression]
expressionsWithInDocument n refs (Doc secs) = expressionsWithInSections n refs secs


--formerly exprssEqualTo

-- expressionsEqualToInExpressions :: [Expression] -> [Expression] -> [Expression]
-- expressionsEqualToInExpressions [] _ = []
-- expressionsEqualToInExpressions _ [] = []
-- expressionsEqualToInExpressions refs (exp:exps)
--   | any (== exp) refs = exp : expressionsEqualToInExpressions refs exps
--   | otherwise         = expressionsEqualToInExpressions refs exps
--
-- expressionsEqualToInInlines :: [Expression] -> Int -> [Inline] -> [Expression]
-- expressionsEqualToInInlines refs n inls =
--   expressionsEqualToInExpressions refs $ ngramsInInlines' n inls
--
--NB: `refs` is the list of reference expressions, `inls` the list from which to
--generate expressions of `n` inline elements to search among `refs`.
--
--NB1: we could get rid of the `n` argument if we recurse over the length of `refs`
--and at each point we generate expressions of the same length. See `targetExpressionInlines`.

expressionsEqualToInInlines :: [Expression] -> [Inline] -> [Expression]
expressionsEqualToInInlines [] _ = []
expressionsEqualToInInlines _ [] = []
expressionsEqualToInInlines (ref:refs) inls =
  let expsFromInls = ngramsInInlines' (expressionLength ref) inls
  in nub $ filter (== ref) expsFromInls ++ expressionsEqualToInInlines refs inls


exps1 :: [Expression]
exps1 = [Expression [Word [None] (0,0) "am",Word [None] (1,170) "turned"],Expression [Word [None] (0,0) "am",Word [None] (7,41) "horrified"],Expression [Word [None] (0,0) "am",Word [None] (7,57) "considered"],Expression [Word [None] (0,0) "am",Word [None] (17,58) "dedicated"],Expression [Word [None] (0,0) "are",Word [None] (1,133) "received"],Expression [Word [None] (0,0) "are",Word [None] (1,170) "turned"],Expression [Word [None] (0,0) "are",Word [None] (7,41) "horrified"],Expression [Word [None] (0,0) "are",Word [None] (7,57) "considered"],Expression [Word [None] (0,0) "are",Word [None] (17,58) "dedicated"],Expression [Word [None] (0,0) "been",Word [None] (1,133) "received"]]

inls11 :: [Inline]
inls11 = [ Word [None] (0,0) "I", Word [None] (0,0) "don't",Word [None] (0,0) "like", Word [None] (0,0) "sprouts", Word [None] (0,0) "so", Word [None] (0,0) "when", Word [None] (0,0) "I", Word [None] (0,0) "was", Word [None] (0,0) "given", Word [None] (0,0) "a", Word [None] (0,0) "full", Word [None] (0,0) "plate", Word [None] (0,0) "of", Word [None] (0,0) "the", Word [None] (0,0) "horrible", Word [None] (0,0) "vegetable", Word [None] (0,0) "I", Word [None] (0,0) "turned", Word [None] (0,0) "green", Word [None] (0,0) "are", Word [None] (0,0) "considered", Word [None] (0,0) "a", Word [None] (0,0) "pest", Word [None] (0,0) "in", Word [None] (0,0) "Pacific", Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "am", Word [None] (0,0) "horrified", Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "are", Word [None] (0,0) "dedicated", Word [None] (0,0) "blah", Word [None] (0,0) "blah"]


expressionsEqualToInSentence :: [Expression] -> Sentence -> [Expression]
expressionsEqualToInSentence refs (Sentence inls) = expressionsEqualToInInlines refs inls
expressionsEqualToInSentence _ _ = []

expressionsEqualToInSentences :: [Expression] -> [Sentence] -> [Expression]
expressionsEqualToInSentences refs snts = concatMap (expressionsEqualToInSentence refs) snts

-- expressionsEqualToInExample :: [Expression] -> Example -> [Expression]
-- expressionsEqualToInExample refs (MainEx _ _ _ snts) = expressionsEqualToInSentences refs snts
-- expressionsEqualToInExample refs (SubEx _ _ _ snts) = expressionsEqualToInSentences refs snts
--
-- expressionsEqualToInExamples :: [Expression] -> [Example] -> [Expression]
-- expressionsEqualToInExamples refs exs = concatMap (expressionsEqualToInExample refs) exs

expressionsEqualToInParaPart :: [Expression] -> ParaPart -> [Expression]
expressionsEqualToInParaPart refs snt@(Sentence _) = expressionsEqualToInSentence refs snt
expressionsEqualToInParaPart refs (ParaFtn _ snts) = expressionsEqualToInSentences refs snts
expressionsEqualToInParaPart refs (Inlines inls) = expressionsEqualToInInlines refs inls
expressionsEqualToInParaPart refs (Caption prts) = expressionsEqualToInParaParts refs prts
expressionsEqualToInParaPart _ _ = []

expressionsEqualToInParaParts :: [Expression] -> [ParaPart] -> [Expression]
expressionsEqualToInParaParts refs prts = concatMap (expressionsEqualToInParaPart refs) prts

expressionsEqualToInParagraph :: [Expression] -> Paragraph -> [Expression]
expressionsEqualToInParagraph refs (Para prts) = expressionsEqualToInParaParts refs prts
expressionsEqualToInParagraph _ _ = []

expressionsEqualToInBlock :: [Expression] -> Block -> [Expression]
expressionsEqualToInBlock refs par@(Para _) = expressionsEqualToInParagraph refs par
expressionsEqualToInBlock refs (Footnote _ blcs) = expressionsEqualToInBlocks refs blcs
expressionsEqualToInBlock refs (BlockEx exs) =
  expressionsEqualToInParaParts refs $ unwrapExamples exs
expressionsEqualToInBlock refs (BlockQuotes blcs) = expressionsEqualToInBlocks refs blcs
expressionsEqualToInBlock refs (Table prts) = expressionsEqualToInParaParts refs prts
expressionsEqualToInBlock _ _ = []

expressionsEqualToInBlocks :: [Expression] -> [Block] -> [Expression]
expressionsEqualToInBlocks refs blcs = concatMap (expressionsEqualToInBlock refs) blcs

expressionsEqualToInSection :: [Expression] -> Section -> [Expression]
expressionsEqualToInSection refs (Section _ _ (Title inls) bdy subs) =
  expressionsEqualToInInlines refs inls ++
  expressionsEqualToInBlocks refs (secblcs . secbdy $ bdy) ++
  expressionsEqualToInSections refs subs
expressionsEqualToInSection refs (SecBlocks blcs) = expressionsEqualToInBlocks refs blcs
expressionsEqualToInSection refs (Meta mp) = titleExpressions mp refs ++ abstractExpressions mp refs

--meta fields

titleExpressions :: M.Map MetaKey MetaValue -> [Expression] -> [Expression]
titleExpressions mp refs = procTitle mp (expressionsEqualToInInlines refs) []

abstractExpressions :: M.Map MetaKey MetaValue -> [Expression] -> [Expression]
abstractExpressions mp refs = procAbstract mp (expressionsEqualToInBlocks refs) []

expressionsEqualToInSections :: [Expression] -> [Section] -> [Expression]
expressionsEqualToInSections refs secs = concatMap (expressionsEqualToInSection refs) secs

expressionsEqualToInDocument :: [Expression] -> Document -> [Expression]
expressionsEqualToInDocument refs (Doc secs) = expressionsEqualToInSections refs secs


--expression length

expressionLength :: Expression -> Int
expressionLength (Expression inls) = length inls

averageExprLength :: [Expression] -> Int
averageExprLength exps = (sum $ map expressionLength exps) `div` length exps

-- defaultPos :: SourcePos
-- defaultPos = initialPos "defaultPos-function-defined-in-Internal"

-- mkExpr :: [Inline] -> Expression
-- mkExpr inls = Expression inls

mkExprs :: [[Inline]] -> [Expression]
mkExprs inlss = fmap Expression inlss

noOverlapOf :: Int -> [Expression] -> [Expression]
noOverlapOf _ [] = []
noOverlapOf _ [e] = [e]
noOverlapOf n (e:es)
  | length overlap >= n = noOverlapOf n (e : tail es)
  | otherwise           = e : noOverlapOf n es
  where
    overlap = intersectExpressions e (head es)

noOverlapOf' :: Int -> [Word] -> [Expression] -> [Expression]
noOverlapOf' _ _ [] = []
noOverlapOf' _ _ [e] = [e]
noOverlapOf' n refs (e:es)
  | length (intersect overlap refs) >= n
    = noOverlapOf' n refs (e : tail es)
  | otherwise
    = e : noOverlapOf' n refs es
  where
    overlap = intersectExpressions e (head es)

iexp1, iexp2, iexp3, iexp4 :: Expression
iexp1 = Expression [Word [None] (1,0) "In",Word [None] (1,3) "the",Word [None] (1,7) "middle",Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this"]
iexp2 = Expression [Word [None] (1,3) "the",Word [None] (1,7) "middle",Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this",Word [None] (1,27) "after"]
iexp3 = Expression [Word [None] (1,7) "middle",Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this",Word [None] (1,27) "after",Word [None] (1,33) "all"]
iexp4 = Expression [Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this",Word [None] (1,27) "after",Word [None] (1,33) "all",Word [None] (1,37) "that"]
inter12 :: [Inline]
inter12 = intersectExpressions iexp2 iexp3

intersectExpressions :: Expression -> Expression -> [Inline]
intersectExpressions (Expression inls1) (Expression inls2) =
  mkWords $ intersect (unwrapInlines inls1) (unwrapInlines inls2)

