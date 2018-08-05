module Abac.Traverse.Ngrams where


import qualified Data.Map.Strict as M


import Abac.Types.ParserTypes

import Abac.Traverse.Internal
import Abac.Traverse.Predicates



--formerly ngram (note that there are two possible verions: inline-based and expression-based;
--we go with the latter)

ngramsInInlines :: Int -> [Inline] -> [[Inline]]
ngramsInInlines 0 _ = []
ngramsInInlines _ [] = []
ngramsInInlines n inls' =
  let inls = filter wordlike $ removeInlineWrapers inls'
  in filter (not . null) $
    if length inls >= n
       then take n inls : ngramsInInlines n (tail inls)
       else ngramsInInlines n (tail inls)

-- NB: you should also include a no-overlap condition: no expression with overlapping
-- words (inlines).

ngramsInInlines' :: Int -> [Inline] -> [Expression]
ngramsInInlines' n inls = Expression <$> ngramsInInlines n inls

ngramsInSent :: Int -> Sentence -> [Expression]
ngramsInSent n (Sentence inls) = ngramsInInlines' n inls
ngramsInSent _ _ = []

ngramsInParaPart :: Int -> ParaPart -> [[Expression]]
ngramsInParaPart n s@(Sentence _) = [ngramsInSent n s]
ngramsInParaPart n (Inlines inls) = [ngramsInInlines' n inls]
ngramsInParaPart n (ParaFtn _ sents) = ngramsInSent n <$> sents
ngramsInParaPart n (Caption parpts) = (concat . ngramsInParaPart n) <$> parpts
ngramsInParaPart _ _ = []


ngramsInParaPartFlat :: Int -> ParaPart -> [Expression]
ngramsInParaPartFlat n p = concat $ ngramsInParaPart n p

-- ngramsInExample :: Int -> Example -> [[Expression]]
-- ngramsInExample n ex@(MainEx _ _ _ snts) = fmap (ngramsInSent n) snts
-- ngramsInExample n ex@(SubEx _ _ _ snts) = fmap (ngramsInSent n) snts


--formerly ngramPara

ngramsInBlock :: Int -> Block -> [[[Expression]]]
ngramsInBlock n (Para prts) = ngramsInParaPart n <$> prts
ngramsInBlock n (Footnote _ blcs) = concatMap (ngramsInBlock n) blcs
ngramsInBlock n (BlockEx exs) = ngramsInParaPart n <$> unwrapExamples exs
ngramsInBlock n (BlockQuotes blcs) = concatMap (ngramsInBlock n) blcs
ngramsInBlock n (BlockQuote par) = ngramsInBlock n par
ngramsInBlock _ _ = []


ngramsInBlockFlat :: Int -> Block -> [Expression]
ngramsInBlockFlat n blc = concat . concat $ ngramsInBlock n blc

ngramsInBlocks :: Int -> [Block] -> [Expression]
ngramsInBlocks n blcs = concatMap (ngramsInBlockFlat n) blcs

ngramsInSection :: Int -> Section -> [Expression]
ngramsInSection n (Section _ _ ttl bdy subs) =
  let titleExpressions' m (Title inls) = ngramsInSent m (Sentence inls)
      bodyExpressions m body = ngramsInBlocks m (secblcs . secbdy $ body)
  in titleExpressions' n ttl ++ bodyExpressions n bdy ++ concatMap (ngramsInSection n) subs
ngramsInSection n (SecBlocks blcs) = ngramsInBlocks n blcs
ngramsInSection n (Meta mp) = titleAllExpressions n mp ++ abstractAllExpressions n mp

--meta fields
abstractAllExpressions :: Int -> M.Map MetaKey MetaValue -> [Expression]
abstractAllExpressions n mp = procAbstract mp (ngramsInBlocks n) []

titleAllExpressions :: Int -> M.Map MetaKey MetaValue -> [Expression]
titleAllExpressions n mp = procTitle mp (ngramsInSent n . Sentence) []


-- ngrams in the entire document
ngramsInDocument :: Int -> Document -> [Expression]
ngramsInDocument n (Doc secs) = concatMap (ngramsInSection n) secs

