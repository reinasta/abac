module Abac.Traverse.Quantify where

import Prelude hiding (Word)


import Abac.Types.ParserTypes
--import Abac.Internal

--import Abac.Traverse.Searchable

import Abac.Traverse.Internal
import Abac.Traverse.Ngrams
import Abac.Traverse.Expressions

--Quantification: counts and percentages
--------------------------------------

--NB: ideally, all the following functions should be written in terms of two kinds of
--functions already defined above, namely (i) the total count functions `countWordsIn...`
--and (ii) the functions outputting problematic inlines, e.g. `wordsWithIn...` and
--`expressionsEqualTo...`. Below I mark out the functions that follow this idea and
--sometimes give a (primed') version of the function that adheres to (i-ii).

targetWordsInlines :: [Suffix] -> [Inline] -> Int -- using functions above
targetWordsInlines ends inls = countWordsInInlines $ wordsWithInInlines ends inls

proportionWordsInlines :: [Suffix] -> [Inline] -> Double -- using functions above
proportionWordsInlines ends inls =
  let percentage = numerator %> denominator
      numerator = fromIntegral $ targetWordsInlines ends inls
      denominator = fromIntegral $ countWordsInInlines inls
  in  if isNaN percentage then 0 else percentage

targetWordsSentence :: [Suffix] -> Sentence -> Int
targetWordsSentence ends (Sentence inls) = targetWordsInlines ends inls
targetWordsSentence _ _ = 0

proportionWordsSentence :: [Suffix] -> Sentence -> Double
proportionWordsSentence ends (Sentence inls) = proportionWordsInlines ends inls
proportionWordsSentence _ _ = 0

targetWordsSentences :: [Suffix] -> [Sentence] -> Int
targetWordsSentences ends snts = sum $ targetWordsSentence ends <$> snts

proportionWordsSentences :: [Suffix] -> [Sentence] -> Double
proportionWordsSentences ends snts = average $ (proportionWordsSentence ends) <$> snts

-- targetWordsExample :: [Suffix] -> Example -> Int
-- targetWordsExample ends (MainEx _ _ _ snts) = targetWordsSentences ends snts
-- targetWordsExample ends (SubEx _ _ _ snts) = targetWordsSentences ends snts
--
-- proportionWordsExample :: [Suffix] -> Example -> Double
-- proportionWordsExample ends (MainEx _ _ _ snts) = proportionWordsSentences ends snts
-- proportionWordsExample ends (SubEx _ _ _ snts) = proportionWordsSentences ends snts
--
-- targetWordsExamples :: [Suffix] -> [Example] -> Int
-- targetWordsExamples ends exs = sum $ targetWordsExample ends <$> exs

-- proportionWordsExamples :: [Suffix] -> [Example] -> Double
-- proportionWordsExamples ends exs = average $ (proportionWordsExample ends) <$> exs

targetWordsParaPart :: [Suffix] -> ParaPart -> Int
targetWordsParaPart ends snt@(Sentence _) = targetWordsSentence ends snt
targetWordsParaPart ends (ParaFtn _ snts) = targetWordsSentences ends snts
targetWordsParaPart ends (Inlines inls) = targetWordsInlines ends inls
targetWordsParaPart ends (Caption prts) = targetWordsParaParts ends prts
targetWordsParaPart _ _ = 0

proportionWordsParaPart :: [Suffix] -> ParaPart -> Double
proportionWordsParaPart ends snt@(Sentence _) = proportionWordsParaPart ends snt
proportionWordsParaPart ends (ParaFtn _ snts) = proportionWordsSentences ends snts
proportionWordsParaPart ends (Inlines inls) = proportionWordsInlines ends inls
proportionWordsParaPart ends (Caption prts) = proportionWordsParaParts ends prts
proportionWordsParaPart _ _ = ignored

targetWordsParaParts :: [Suffix] -> [ParaPart] -> Int
targetWordsParaParts ends prts = sum $ targetWordsParaPart ends <$> prts

proportionWordsParaParts :: [Suffix] -> [ParaPart] -> Double
proportionWordsParaParts ends prts = average $ proportionWordsParaPart ends <$> prts

targetWordsParagraph :: [Suffix] -> Paragraph -> Int
targetWordsParagraph ends (Para prts) = targetWordsParaParts ends prts
targetWordsParagraph _ _ = 0

proportionWordsParagraph :: [Suffix] -> Paragraph -> Double
proportionWordsParagraph ends (Para prts) = proportionWordsParaParts ends prts
proportionWordsParagraph _ _ = 0

targetWordsBlock :: [Suffix] -> Block -> Int
targetWordsBlock ends par@(Para _) = targetWordsParagraph ends par
targetWordsBlock ends (Footnote _ blcs) = targetWordsBlocks ends blcs
targetWordsBlock ends (BlockEx exs) = targetWordsParaParts ends $ unwrapExamples exs
targetWordsBlock ends (BlockQuotes blcs) = targetWordsBlocks ends blcs
targetWordsBlock _ (BlockTech _ _ _) = 0
targetWordsBlock _ (LinkRef _ _ _) = 0
targetWordsBlock _ (ImageRef _ _ _) = 0
targetWordsBlock ends (Table prts) = targetWordsParaParts ends prts
targetWordsBlock _ _ = 0

proportionWordsBlock :: [Suffix] -> Block -> Double
proportionWordsBlock ends par@(Para _) = proportionWordsParagraph ends par
proportionWordsBlock ends (Footnote _ blcs) = proportionWordsBlocks ends blcs
proportionWordsBlock ends (BlockEx exs) = proportionWordsParaParts ends $ unwrapExamples exs
proportionWordsBlock ends (BlockQuotes blcs) = proportionWordsBlocks ends blcs
proportionWordsBlock _ (BlockTech _ _ _) = ignored
proportionWordsBlock _ (LinkRef _ _ _) = ignored
proportionWordsBlock _ (ImageRef _ _ _) = ignored
proportionWordsBlock ends (Table prts) = proportionWordsParaParts ends prts
proportionWordsBlock _ _ = 0

targetWordsBlocks :: [Suffix] -> [Block] -> Int
targetWordsBlocks ends blcs = sum $ targetWordsBlock ends <$> blcs

proportionWordsBlocks :: [Suffix] -> [Block] -> Double
proportionWordsBlocks ends blcs = average $ proportionWordsBlock ends <$> blcs

targetWordsSection :: [Suffix] -> Section -> Int
targetWordsSection ends (Section _ _ (Title inls) bdy secs) =
  targetWordsInlines ends inls +
  targetWordsBlocks ends (secblcs . secbdy $ bdy) +
  targetWordsSections ends secs
targetWordsSection ends (SecBlocks blcs) = targetWordsBlocks ends blcs
targetWordsSection ends meta@(Meta _) = length $ wordsWithInSection ends meta -- using functions above

proportionWordsSection :: [Suffix] -> Section -> Double
proportionWordsSection ends sec =
  fromIntegral (targetWordsSection ends sec) %> fromIntegral (countWordsInSection sec)

targetWordsSections :: [Suffix] -> [Section] -> Int
targetWordsSections ends secs = sum $ targetWordsSection ends <$> secs

proportionWordsSections :: [Suffix] -> [Section] -> Double
proportionWordsSections ends secs =
  fromIntegral (targetWordsSections ends secs) %> fromIntegral (countWordsInSections secs)

targetWordsDocument :: [Suffix] -> Document -> Int
targetWordsDocument ends (Doc secs) = targetWordsSections ends secs

proportionWordsDocument :: [Suffix] -> Document -> Double
proportionWordsDocument ends (Doc secs) = proportionWordsSections ends secs

--words equal to

targetEqWordsInlines :: [Word] -> [Inline] -> Int
targetEqWordsInlines refs inls = countWordsInInlines $ wordsEqualToInInlines refs inls

proportionEqWordsInlines :: [Word] -> [Inline] -> Double
proportionEqWordsInlines refs inls =
  let percentage = numerator %> denominator
      numerator = fromIntegral (targetEqWordsInlines refs inls)
      denominator = fromIntegral (countWordsInInlines inls)
  in  if isNaN percentage then 0 else percentage

targetEqWordsSentence :: [Word] -> Sentence -> Int
targetEqWordsSentence refs (Sentence inls) = targetEqWordsInlines refs inls
targetEqWordsSentence _ _ = 0

proportionEqWordsSentence :: [Word] -> Sentence -> Double
proportionEqWordsSentence refs (Sentence inls) = proportionEqWordsInlines refs inls
proportionEqWordsSentence _ _ = 0

targetEqWordsSentences :: [Word] -> [Sentence] -> Int
targetEqWordsSentences refs sents = sum $ targetEqWordsSentence refs <$> sents

proportionEqWordsSentences :: [Word] -> [Sentence] -> Double
proportionEqWordsSentences refs sents = average $ proportionEqWordsSentence refs <$> sents

targetEqWordsParaPart :: [Word] -> ParaPart -> Int
targetEqWordsParaPart refs sent@(Sentence _) = targetEqWordsSentence refs sent
targetEqWordsParaPart refs (ParaFtn _ sents) = targetEqWordsSentences refs sents
targetEqWordsParaPart refs (Inlines inls) = targetEqWordsInlines refs inls
targetEqWordsParaPart refs (Caption prts) = targetEqWordsParaParts refs prts
targetEqWordsParaPart _ _ = 0

proportionEqWordsParaPart :: [Word] -> ParaPart -> Double
proportionEqWordsParaPart refs sent@(Sentence _) = proportionEqWordsSentence refs sent
proportionEqWordsParaPart refs (ParaFtn _ sents) = proportionEqWordsSentences refs sents
proportionEqWordsParaPart refs (Inlines inls) = proportionEqWordsInlines refs inls
proportionEqWordsParaPart refs (Caption prts) = proportionEqWordsParaParts refs prts
proportionEqWordsParaPart _ _ = ignored

targetEqWordsParaParts :: [Word] -> [ParaPart] -> Int
targetEqWordsParaParts refs prts = sum $ targetEqWordsParaPart refs <$> prts

proportionEqWordsParaParts :: [Word] -> [ParaPart] -> Double
proportionEqWordsParaParts refs prts = average $ proportionEqWordsParaPart refs <$> prts

-- targetEqWordsExample :: [Word] -> Example -> Int
-- targetEqWordsExample refs (MainEx _ _ _ sents) = targetEqWordsSentences refs sents
-- targetEqWordsExample refs (SubEx _ _ _ sents) = targetEqWordsSentences refs sents
--
-- proportionEqWordsExample :: [Word] -> Example -> Double
-- proportionEqWordsExample refs (MainEx _ _ _ sents) = proportionEqWordsSentences refs sents
-- proportionEqWordsExample refs (SubEx _ _ _ sents) = proportionEqWordsSentences refs sents
--
-- targetEqWordsExamples :: [Word] -> [Example] -> Int
-- targetEqWordsExamples refs exs = sum $ targetEqWordsExample refs <$> exs
--
-- proportionEqWordsExamples :: [Word] -> [Example] -> Double
-- proportionEqWordsExamples refs exs = average $ proportionEqWordsExample refs <$> exs

targetEqWordsParagraph :: [Word] -> Paragraph -> Int
targetEqWordsParagraph refs (Para prts) = targetEqWordsParaParts refs prts
targetEqWordsParagraph _ _ = 0

proportionEqWordsParagraph :: [Word] -> Paragraph -> Double
proportionEqWordsParagraph refs (Para prts) = proportionEqWordsParaParts refs prts
proportionEqWordsParagraph _ _ = 0

targetEqWordsBlock :: [Word] -> Block -> Int
targetEqWordsBlock refs par@(Para _) = targetEqWordsParagraph refs par
targetEqWordsBlock refs (Footnote _ blcs) = targetEqWordsBlocks refs blcs
targetEqWordsBlock refs (BlockEx exs) = targetEqWordsParaParts refs $ unwrapExamples exs
targetEqWordsBlock refs (BlockQuotes blcs) = targetEqWordsBlocks refs blcs
targetEqWordsBlock _ (BlockTech _ _ _) = 0
targetEqWordsBlock _ (LinkRef _ _ _) = 0
targetEqWordsBlock _ (ImageRef _ _ _) = 0
targetEqWordsBlock refs (Table prts) = targetEqWordsParaParts refs prts
targetEqWordsBlock _ _ = 0

proportionEqWordsBlock :: [Word] -> Block -> Double
proportionEqWordsBlock refs par@(Para _) = proportionEqWordsParagraph refs par
proportionEqWordsBlock refs (Footnote _ blcs) = proportionEqWordsBlocks refs blcs
proportionEqWordsBlock refs (BlockEx exs) = proportionEqWordsParaParts refs $ unwrapExamples exs
proportionEqWordsBlock refs (BlockQuotes blcs) = proportionEqWordsBlocks refs blcs
proportionEqWordsBlock _ (BlockTech _ _ _) = ignored
proportionEqWordsBlock _ (LinkRef _ _ _) = ignored
proportionEqWordsBlock _ (ImageRef _ _ _) = ignored
proportionEqWordsBlock refs (Table prts) = proportionEqWordsParaParts refs prts
proportionEqWordsBlock _ _ = 0

targetEqWordsBlocks :: [Word] -> [Block] -> Int
targetEqWordsBlocks refs blcs = sum $ targetEqWordsBlock refs <$> blcs

proportionEqWordsBlocks :: [Word] -> [Block] -> Double
proportionEqWordsBlocks refs blcs = average $ proportionEqWordsBlock refs <$> blcs

targetEqWordsSection :: [Word] -> Section -> Int
targetEqWordsSection refs sec =
  length $ wordsEqualToInSection refs sec -- using section function above, instead of inline functions

proportionEqWordsSection :: [Word] -> Section -> Double
proportionEqWordsSection refs sec =  -- using section functions above instead of inline functions
  let percent = fromIntegral (targetEqWordsSection refs sec) %> fromIntegral (countWordsInSection sec)
  in  if isNaN percent then 0 else percent

targetEqWordsSections :: [Word] -> [Section] -> Int
targetEqWordsSections refs secs = sum $ targetEqWordsSection refs <$> secs

proportionEqWordsSections :: [Word] -> [Section] -> Double
proportionEqWordsSections refs secs = average $ proportionEqWordsSection refs <$> secs

targetEqWordsDocument :: [Word] -> Document -> Int
targetEqWordsDocument refs (Doc secs) = targetEqWordsSections refs secs

proportionEqWordsDocument :: [Word] -> Document -> Double
proportionEqWordsDocument refs (Doc secs) = proportionEqWordsSections refs secs


--expressions

targetExpressionsInlines :: [Expression] -> [Inline] -> Int
targetExpressionsInlines [] _ = 0
targetExpressionsInlines _  [] = 0
targetExpressionsInlines (r:rs) inls =
  let expsFromInls = ngramsInInlines' (expressionLength r) inls
  in (length $ filter (== r) expsFromInls) + targetExpressionsInlines rs inls

proportionExpressionsInlines :: [Expression] -> [Inline] -> Double
proportionExpressionsInlines refs inls =
  let percentage = numerator %> denominator
      numerator = fromIntegral (targetExpressionsInlines refs inls)
      denominator = fromIntegral (length $ ngramsInInlines' (averageExprLength refs) inls)
  in  if isNaN percentage then 0 else percentage


--We could have simply done: expressionsEqualToInInlines refs (expressionLength $ head refs) inls.
--But the advantage of taking the length of the expressions as we recurse over the length of the
--list of expressions `refs` is that we can have epressions of different lengths in `refs`. The
--simpler method bounds us to search for expressions of a fixed length (that of the list's head).


targetExpressionsSentence :: [Expression] -> Sentence -> Int
targetExpressionsSentence refs (Sentence inls) = targetExpressionsInlines refs inls
targetExpressionsSentence _ _ = 0

proportionExpressionsSentence :: [Expression] -> Sentence -> Double
proportionExpressionsSentence refs (Sentence inls) = proportionExpressionsInlines refs inls
proportionExpressionsSentence _ _ = 0

targetExpressionsSentences :: [Expression] -> [Sentence] -> Int
targetExpressionsSentences refs sents = sum $ targetExpressionsSentence refs <$> sents

proportionExpressionsSentences :: [Expression] -> [Sentence] -> Double
proportionExpressionsSentences refs sents = average $ proportionExpressionsSentence refs <$> sents

-- targetExpressionsExample :: [Expression] -> Example -> Int
-- targetExpressionsExample refs (MainEx _ _ _ sents) = targetExpressionsSentences refs sents
-- targetExpressionsExample refs (SubEx _ _ _ sents) = targetExpressionsSentences refs sents
--
-- proportionExpressionsExample :: [Expression] -> Example -> Double
-- proportionExpressionsExample refs (MainEx _ _ _ sents) = proportionExpressionsSentences refs sents
-- proportionExpressionsExample refs (SubEx _ _ _ sents) = proportionExpressionsSentences refs sents
--
-- targetExpressionsExamples :: [Expression] -> [Example] -> Int
-- targetExpressionsExamples refs exs = sum $ targetExpressionsExample refs <$> exs
--
-- proportionExpressionsExamples :: [Expression] -> [Example] -> Double
-- proportionExpressionsExamples refs exs = average $ proportionExpressionsExample refs <$> exs

targetExpressionsParaPart :: [Expression] -> ParaPart -> Int
targetExpressionsParaPart refs (Sentence inls) = targetExpressionsInlines refs inls
targetExpressionsParaPart refs (ParaFtn _ snts) = targetExpressionsSentences refs snts
targetExpressionsParaPart refs (Inlines inls) = targetExpressionsInlines refs inls
targetExpressionsParaPart refs (Caption prts) = targetExpressionsParaParts refs prts
targetExpressionsParaPart _ _ = 0

proportionExpressionsParaPart :: [Expression] -> ParaPart -> Double
proportionExpressionsParaPart refs sent@(Sentence _) = proportionExpressionsSentence refs sent
proportionExpressionsParaPart refs (ParaFtn _ snts) = proportionExpressionsSentences refs snts
proportionExpressionsParaPart refs (Inlines inls) = proportionExpressionsInlines refs inls
proportionExpressionsParaPart refs (Caption prts) = proportionExpressionsParaParts refs prts
proportionExpressionsParaPart _ _ = ignored


targetExpressionsParaParts :: [Expression] -> [ParaPart] -> Int
targetExpressionsParaParts refs prts = sum $ targetExpressionsParaPart refs <$> prts

proportionExpressionsParaParts :: [Expression] -> [ParaPart] -> Double
proportionExpressionsParaParts refs prts = average $ proportionExpressionsParaPart refs <$> prts

targetExpressionsParagraph :: [Expression] -> Paragraph -> Int
targetExpressionsParagraph refs (Para prts) = targetExpressionsParaParts refs prts
targetExpressionsParagraph _ _ = 0

proportionExpressionsParagraph :: [Expression] -> Paragraph -> Double
proportionExpressionsParagraph refs (Para prts) = proportionExpressionsParaParts refs prts
proportionExpressionsParagraph _ _ = 0

targetExpressionsBlock :: [Expression] -> Block -> Int
targetExpressionsBlock refs par@(Para _) = targetExpressionsParagraph refs par
targetExpressionsBlock refs (Footnote _ blcs) = targetExpressionsBlocks refs blcs
targetExpressionsBlock refs (BlockEx exs) = targetExpressionsParaParts refs $ unwrapExamples exs
targetExpressionsBlock refs (BlockQuotes blcs) = targetExpressionsBlocks refs blcs
targetExpressionsBlock _ (BlockTech _ _ _) = 0
targetExpressionsBlock _ (LinkRef _ _ _) = 0
targetExpressionsBlock _ (ImageRef _ _ _) = 0
targetExpressionsBlock refs (Table prts) = targetExpressionsParaParts refs prts
targetExpressionsBlock refs (BlockComment blcs) = targetExpressionsBlocks refs blcs
targetExpressionsBlock _ _ = 0

proportionExpressionsBlock :: [Expression] -> Block -> Double
proportionExpressionsBlock refs par@(Para _) = proportionExpressionsParagraph refs par
proportionExpressionsBlock refs (Footnote _ blcs) = proportionExpressionsBlocks refs blcs
proportionExpressionsBlock refs (BlockEx exs) =
  proportionExpressionsParaParts refs $ unwrapExamples exs
proportionExpressionsBlock refs (BlockQuotes blcs) = proportionExpressionsBlocks refs blcs
proportionExpressionsBlock _ (BlockTech _ _ _) = ignored
proportionExpressionsBlock _ (LinkRef _ _ _) = ignored
proportionExpressionsBlock _ (ImageRef _ _ _) = ignored
proportionExpressionsBlock refs (Table prts) = proportionExpressionsParaParts refs prts
proportionExpressionsBlock refs (BlockComment blcs) = proportionExpressionsBlocks refs blcs
proportionExpressionsBlock _ _ = 0

targetExpressionsBlocks :: [Expression] -> [Block] -> Int
targetExpressionsBlocks refs blcs = sum $ targetExpressionsBlock refs <$> blcs

proportionExpressionsBlocks :: [Expression] -> [Block] -> Double
proportionExpressionsBlocks refs blcs = average $ proportionExpressionsBlock refs <$> blcs

targetExpressionsSection :: [Expression] -> Section -> Int
targetExpressionsSection refs sec = length $ expressionsEqualToInSection refs sec

proportionExpressionsSection :: [Expression] -> Section -> Double
proportionExpressionsSection refs sec =
  fromIntegral (targetExpressionsSection refs sec) %>
  fromIntegral (length $ ngramsInSection (averageExprLength refs) sec)

targetExpressionsSections :: [Expression] -> [Section] -> Int
targetExpressionsSections refs blcs = sum $ targetExpressionsSection refs <$> blcs

proportionExpressionsSections :: [Expression] -> [Section] -> Double
proportionExpressionsSections refs secs = average $ proportionExpressionsSection refs <$> secs

targetExpressionsDocument :: [Expression] -> Document -> Int
targetExpressionsDocument refs (Doc secs) = targetExpressionsSections refs secs

proportionExpressionsDocument :: [Expression] -> Document -> Double
proportionExpressionsDocument refs (Doc secs) = proportionExpressionsSections refs secs

