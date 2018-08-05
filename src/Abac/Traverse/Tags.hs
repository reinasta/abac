module Abac.Traverse.Tags where

import Abac.Types.ParserTypes
import Abac.Internal

--import Abac.Traverse.Searchable

import Abac.Traverse.Attributes


hasSomeTag :: [Attr] -> Bool
hasSomeTag [] = False
hasSomeTag (attr:attrs) =
  case attr of
    Tag _ -> True
    _     -> hasSomeTag attrs


-- NLP tags

isTagged :: Inline -> Bool
isTagged inl = hasSomeTag $ getAttrs inl

isTaggedWith :: Inline -> Tag -> Bool
isTaggedWith inl tg = tg `elem` (getAttrs inl)

taggedInline :: Inline -> Maybe Inline
taggedInline inl | isTagged inl = Just inl
                 | otherwise    = Nothing

inlineTaggedWith :: Tag -> Inline -> Maybe Inline
inlineTaggedWith tg inl | hasAttrs inl [tg] = Just inl
                        | otherwise         = Nothing

fstInlineTaggedWith :: Tag -> [Inline] -> Maybe Inline
fstInlineTaggedWith tg inls = find isJust (inlineTaggedWith tg <$> inls) >>= \mi -> mi

inlinesTaggedWith :: Tag -> [Inline] -> [Inline]
inlinesTaggedWith tg inls = filter (\inl -> inl `isTaggedWith` tg) inls

lineTaggedWith :: Tag -> LineNo -> Document -> [Inline]
lineTaggedWith tg lno doc = inlinesTaggedWith tg $ wordsAtLineNo lno doc

parnumTaggedWith :: Tag -> Int -> Document -> [Inline]
parnumTaggedWith tg parno doc =
  let mp = getWordLikesFromBlock <$> (mapit $ getBlocks doc)
      minls = M.lookup parno mp
  in  case minls of
        Nothing   -> []
        Just inls -> inlinesTaggedWith tg inls

-- comments are not searched for tagged inlines

paraPartTaggedWith :: Tag -> ParaPart -> [Inline]
paraPartTaggedWith tg prt = inlinesTaggedWith tg (unwrapParaPart' prt)

blockTaggedWith :: Tag -> Block -> [Inline]
blockTaggedWith tg blc = inlinesTaggedWith tg (getWordLikesFromBlock blc)

sectionTaggedWith :: Tag -> Section -> [Inline]
sectionTaggedWith tg sec = inlinesTaggedWith tg (getInlinesFromDiv sec)

documentTaggedWith :: Tag -> Document -> [Inline]
documentTaggedWith tg (Doc scs) = concatMap (sectionTaggedWith tg) scs

taggedInlines :: [Inline] -> [Inline]
taggedInlines inls = filter isTagged inls

taggedParaPart :: ParaPart -> [Inline]
taggedParaPart prt = taggedInlines (unwrapParaPart' prt)

taggedBlock :: Block -> [Inline]
taggedBlock blc = taggedInlines (getWordLikesFromBlock blc)

taggedSection :: Section -> [Inline]
taggedSection sec = taggedInlines (getInlinesFromDiv sec)


-- Parts of speech --
---------------------

--determiners
tagDT :: Attr
tagDT = Tag "DT" :: Attr

determinersInInlines :: [Inline] -> [Inline]
determinersInInlines inls = inlinesTaggedWith tagDT inls

determinersInParaPart :: ParaPart -> [Inline]
determinersInParaPart prt = paraPartTaggedWith tagDT prt

determinersInBlock :: Block -> [Inline]
determinersInBlock blc = blockTaggedWith tagDT blc

determinersInSection :: Section -> [Inline]
determinersInSection sec = sectionTaggedWith tagDT sec

determinersInDocument :: Document -> [Inline]
determinersInDocument (Doc scs) = concatMap (sectionTaggedWith tagDT) scs

--test
determiners1 :: [Inline]
determiners1 = determinersInBlock pnlptag1_result :: [Inline]

-- past participles

tagVBN :: Attr
tagVBN = Tag "VBN" :: Attr

pastParticiplesInInlines :: [Inline] -> [Inline]
pastParticiplesInInlines inls = inlinesTaggedWith tagVBN inls

pastParticiplesInParaPart :: ParaPart -> [Inline]
pastParticiplesInParaPart prt = paraPartTaggedWith tagVBN prt

pastParticiplesInBlock :: Block -> [Inline]
pastParticiplesInBlock blc = blockTaggedWith tagVBN blc

pastParticiplesInSection :: Section -> [Inline]
pastParticiplesInSection sec = sectionTaggedWith tagVBN sec

pastParticiplesInDocument :: Document -> [Inline]
pastParticiplesInDocument (Doc scs) = concatMap (sectionTaggedWith tagVBN) scs

--test
pastParticiples1 :: [Inline]
pastParticiples1 = pastParticiplesInBlock pnlptag1_result :: [Inline]


-- adjectives

tagJJ :: Attr
tagJJ = Tag "JJ" :: Attr

adjectivesInInlines :: [Inline] -> [Inline]
adjectivesInInlines inls = inlinesTaggedWith tagJJ inls

adjectivesInParaPart :: ParaPart -> [Inline]
adjectivesInParaPart prt = paraPartTaggedWith tagJJ prt

adjectivesInBlock :: Block -> [Inline]
adjectivesInBlock blc = blockTaggedWith tagJJ blc

adjectivesInSection :: Section -> [Inline]
adjectivesInSection sec = sectionTaggedWith tagJJ sec

adjectivesInDocument :: Document -> [Inline]
adjectivesInDocument (Doc scs) = concatMap (sectionTaggedWith tagJJ) scs

--test
adjectives1 :: [Inline]
adjectives1 = adjectivesInBlock pnlptag1_result :: [Inline]


-- adverbs

tagRB :: Attr
tagRB = Tag "RB" :: Attr

adverbsInInlines :: [Inline] -> [Inline]
adverbsInInlines inls = inlinesTaggedWith tagRB inls

adverbsInParaPart :: ParaPart -> [Inline]
adverbsInParaPart prt = paraPartTaggedWith tagRB prt

adverbsInBlock :: Block -> [Inline]
adverbsInBlock blc = blockTaggedWith tagRB blc

adverbsInSection :: Section -> [Inline]
adverbsInSection sec = sectionTaggedWith tagRB sec

adverbsInDocument :: Document -> [Inline]
adverbsInDocument (Doc scs) = concatMap (sectionTaggedWith tagRB) scs

--test
adverbs1 :: [Inline]
adverbs1 = adverbsInBlock pnlptag1_result :: [Inline]

-- searching for word-shapes (e.g. suffix- or equality-based) and tags

taggedWordsInParaPart :: [Suffix] -> ParaPart -> [Inline]
taggedWordsInParaPart ends prt = wordsWithInInlines ends $
  adverbsInParaPart prt ++ adjectivesInParaPart prt

taggedWordsInBlock :: [Suffix] -> Block -> [Inline]
taggedWordsInBlock ends blc = wordsWithInInlines ends $
  adverbsInBlock blc ++ adjectivesInBlock blc

--test
adwords_tagged1 :: [Inline]
adwords_tagged1 = taggedWordsInBlock ends pnlptag1_result
  where
    ends = mkWords
      ["able", "ac", "al", "ant", "ary", "ent", "ful",
       "ible", "ic", "ive", "less", "ous"] :: [Suffix]


pnlptag1_result :: Block
pnlptag1_result = Para [Sentence [Word [Tag "IN",None] (1,0) "In",Space,Word [Tag "DT",None] (1,3) "the",Space,Word [Tag "NN",None] (1,7) "middle",Space,Word [Tag "IN",None] (1,14) "of",Space,Word [Tag "PDT",None] (1,17) "all",Space,Word [Tag "DT",None] (1,21) "this",Space,Word [Tag "NN",None] (1,26) "hullabaloo",Punct InSentence ",",Space,Word [Tag "IN",None] (1,38) "after",Space,Word [Tag "DT",None] (1,44) "all",Space,Word [Tag "NN",None] (1,48) "that",Space,Word [Tag "NN",None] (1,53) "struggle",Punct InSentence ",",Space,Word [Tag "PRP",None] (1,63) "I",Space,Word [Tag "VBD",None] (1,65) "was",Space,Word [Tag "VBN",None] (1,69) "horrified",Punct EndSentence ".",Space],Sentence [Word [Tag "NN",None] (1,80) "Sort",Newline,Word [Tag "IN",None] (2,0) "of",Punct EndSentence ".",Space],Sentence [Word [Tag "DT",None] (2,4) "The",Space,Word [Tag "NN",None] (2,8) "merchandise",Space,Word [Tag "VBZ",None] (2,20) "has",Space,Word [Tag "VBN",None] (2,24) "been",Space,Word [Tag "VBN",None] (2,29) "returned",Space,Word [Tag "IN",None] (2,38) "in",Space,Word [Tag "DT",None] (2,41) "the",Space,Word [Tag "NN",None] (2,45) "depot",Space,Word [Tag "IN",None] (2,51) "of",Space,Word [Tag "NNP",None] (2,54) "Berlin",Space,Word [Tag "VBD",None] (2,61) "untouched",Punct InSentence ",",Space,Word [Tag "WDT",None] (2,72) "which",Space,Word [Tag "VBZ",None] (2,78) "is",Space,Word [Tag "JJ",None] (2,81) "abominable",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (2,93) "It",Space,Word [Tag "VBD",None] (2,96) "was",Space,Word [Tag "RB",None] (2,100) "badly",Space,Word [Tag "VBN",None] (2,106) "used",Punct EndSentence ".",Space],Sentence [Word [Tag "NOTAG",None] (2,112) "In",Newline,Word [Tag "NOTAG",None] (3,0) "the",Space,Word [Tag "NOTAG",None] (3,4) "depot",Space,Word [Tag "NOTAG",None] (3,10) "in",Space,Word [Tag "NOTAG",None] (3,13) "Munich",Space,Word [Tag "NOTAG",None] (3,20) "things",Space,Word [Tag "NOTAG",None] (3,27) "were",Space,Word [Tag "NOTAG",None] (3,32) "even",Space,Word [Tag "NOTAG",None] (3,37) "worse",Punct InSentence ":",Space,Word [Tag "NOTAG",None] (3,44) "there",Space,Word [Tag "NOTAG",None] (3,50) "were",Space,Word [Tag "NOTAG",None] (3,55) "people",Space,Word [Tag "NOTAG",None] (3,62) "who",Space,Word [Tag "NOTAG",None] (3,66) "were",Space,Word [Tag "NOTAG",None] (3,71) "being",Space,Word [Tag "NOTAG",None] (3,77) "punished",Space,Word [Tag "NOTAG",None] (3,86) "for",Space,Word [Tag "NOTAG",None] (3,90) "eating",Space,Word [Tag "NOTAG",None] (3,97) "the",Space,Word [Tag "NOTAG",None] (3,101) "only",Space,Word [Tag "NOTAG",None] (3,106) "remaining",Space,Word [Tag "NOTAG",None] (3,116) "food",Space,Word [Tag "NOTAG",None] (3,121) "supplies",Punct EndSentence ".",Null],Sentence [Word [Tag "DT",None] (4,0) "That",Space,Word [Tag "VBD",None] (4,5) "was",Space,Word [Tag "JJ",None] (4,9) "unacceptable",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (4,23) "It",Space,Word [Tag "VBD",None] (4,26) "was",Space,Word [Tag "JJ",None] (4,30) "egregious",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (4,41) "I",Space,Word [Tag "VBD",None] (4,43) "was",Space,Word [Tag "RB",None] (4,47) "really",Space,Word [Tag "VBN",None] (4,54) "horrified",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (4,65) "I",Newline,Space,Word [Tag "VBD",None] (5,1) "was",Space,Word [Tag "VBN",None] (5,5) "flabbergasted",Punct EndSentence ".",Space]]


