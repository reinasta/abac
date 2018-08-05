module Abac.Traverse.Words where

import qualified Data.Map.Strict as M
import Prelude hiding (Word)


import Abac.Types.ParserTypes
--import Abac.Internal

import Abac.Traverse.Internal
import Abac.Traverse.Select

--formerly wordsWith


wordsWithInInlines :: [Suffix] -> [Inline] -> [Inline]
wordsWithInInlines ends inls =
  let inls' = removeInlineWrapers inls
  in selectInlinesWith isMySuffixOf ends inls'

-- wordsWithInInlines :: [Suffix] -> [Inline] -> [Inline]
-- wordsWithInInlines ends (inl:inls) =
--   case inl of
--     (Link inls' _)  ->
--       selectInlinesWith isMySuffixOf ends inls' ++ selectInlinesWith isMySuffixOf ends inls
--     (Image inls' _) ->
--       selectInlinesWith isMySuffixOf ends inls' ++ selectInlinesWith isMySuffixOf ends inls
--     _               -> selectInlinesWith isMySuffixOf ends inls

wordsWithInSentence :: [Suffix] -> Sentence -> [Inline]
wordsWithInSentence ends (Sentence inls) = wordsWithInInlines ends inls
wordsWithInSentence _ _ = []

wordsWithInSentences :: [Suffix] -> [Sentence] -> [Inline]
wordsWithInSentences ends snts = concatMap (wordsWithInSentence ends) snts

-- wordsWithInExample :: [Suffix] -> Example -> [Inline]
-- wordsWithInExample ends (MainEx _ _ _ snts) = wordsWithInSentences ends snts
-- wordsWithInExample ends (SubEx _ _ _ snts) = wordsWithInSentences ends snts
--
-- wordsWithInExamples :: [Suffix] -> [Example] -> [Inline]
-- wordsWithInExamples ends exs = concatMap (wordsWithInExample ends) exs

wordsWithInParaPart :: [Suffix] -> ParaPart -> [Inline]
wordsWithInParaPart ends snt@(Sentence _) = wordsWithInSentence ends snt
wordsWithInParaPart ends (ParaFtn _ snts) = wordsWithInSentences ends snts
wordsWithInParaPart ends (Inlines inls) = wordsWithInInlines ends inls
wordsWithInParaPart ends (Caption prts) = wordsWithInParaParts ends prts
wordsWithInParaPart _ _ = []

wordsWithInParaParts :: [Suffix] -> [ParaPart] -> [Inline]
wordsWithInParaParts ends prts = concatMap (wordsWithInParaPart ends) prts

wordsWithInParagraph :: [Suffix] -> Paragraph -> [Inline]
wordsWithInParagraph ends (Para prts) = wordsWithInParaParts ends prts
wordsWithInParagraph _ _ = []

wordsWithInBlock :: [Suffix] -> Block -> [Inline]
wordsWithInBlock ends par@(Para _) = wordsWithInParagraph ends par
wordsWithInBlock ends (Footnote _ blcs) = wordsWithInBlocks ends blcs
wordsWithInBlock ends (BlockEx exs) = concat $ wordsWithInParaPart ends <$> unwrapExamples exs
wordsWithInBlock ends (BlockQuotes blcs) = wordsWithInBlocks ends blcs
wordsWithInBlock ends (Table prts) = wordsWithInParaParts ends prts
wordsWithInBlock _ _ = []

wordsWithInBlocks :: [Suffix] -> [Block] -> [Inline]
wordsWithInBlocks ends blc = concatMap (wordsWithInBlock ends) blc

wordsWithInSection :: [Suffix] -> Section -> [Inline]
wordsWithInSection ends (Section _ _ (Title inls) bdy secs) =
  wordsWithInInlines ends inls ++
  wordsWithInBlocks ends (secblcs . secbdy $ bdy) ++
  wordsWithInSections ends secs
wordsWithInSection ends (SecBlocks blcs) = wordsWithInBlocks ends blcs
wordsWithInSection ends (Meta mp) = titleWordsWithInlines mp ends ++ abstractWordsWithInlines mp ends

--meta fields
titleWordsWithInlines :: M.Map MetaKey MetaValue -> [Suffix] -> [Inline]
titleWordsWithInlines mp ends = procTitle mp (wordsWithInInlines ends) []

abstractWordsWithInlines :: M.Map MetaKey MetaValue -> [Suffix] -> [Inline]
abstractWordsWithInlines mp ends = procAbstract mp (wordsWithInBlocks ends) []

wordsWithInSections :: [Suffix] -> [Section] -> [Inline]
wordsWithInSections ends secs = concatMap (wordsWithInSection ends) secs

wordsWithInDocument :: [Suffix] -> Document -> [Inline]
wordsWithInDocument ends (Doc secs) = wordsWithInSections ends secs

--formerly wordsEqualTo

wordsEqualToInInlines :: [Word] -> [Inline] -> [Inline]
wordsEqualToInInlines refs inls =
  let inls' = removeInlineWrapers inls in selectInlinesWith (==) refs inls'

wordsEqualToInSentence :: [Word] -> Sentence -> [Inline]
wordsEqualToInSentence refs (Sentence inls) = wordsEqualToInInlines refs inls
wordsEqualToInSentence _ _ = []

wordsEqualToInSentences :: [Word] -> [Sentence] -> [Inline]
wordsEqualToInSentences refs snts = concatMap (wordsEqualToInSentence refs) snts

-- wordsEqualToInExample :: [Word] -> Example -> [Inline]
-- wordsEqualToInExample refs (MainEx _ _ _ snts) = wordsEqualToInSentences refs snts
-- wordsEqualToInExample refs (SubEx _ _ _ snts) = wordsEqualToInSentences refs snts
--
-- wordsEqualToInExamples :: [Word] -> [Example] -> [Inline]
-- wordsEqualToInExamples refs exs = concatMap (wordsEqualToInExample refs) exs

wordsEqualToInParaPart :: [Word] -> ParaPart -> [Inline]
wordsEqualToInParaPart refs (Sentence inls) = wordsEqualToInInlines refs inls
wordsEqualToInParaPart refs (ParaFtn _ snts) = wordsEqualToInSentences refs snts
wordsEqualToInParaPart refs (Inlines inls) = wordsEqualToInInlines refs inls
wordsEqualToInParaPart refs (Caption prts) = wordsEqualToInParaParts refs prts
wordsEqualToInParaPart _ _ = []


wordsEqualToInParaParts :: [Word] -> [ParaPart] -> [Inline]
wordsEqualToInParaParts refs prts = concatMap (wordsEqualToInParaPart refs) prts

wordsEqualToInParagraph :: [Word] -> Paragraph -> [Inline]
wordsEqualToInParagraph refs (Para prts) = wordsEqualToInParaParts refs prts
wordsEqualToInParagraph _ _ = []

wordsEqualToInBlock :: [Word] -> Block -> [Inline]
wordsEqualToInBlock refs par@(Para _) = wordsEqualToInParagraph refs par
wordsEqualToInBlock refs (Footnote _ blcs) = wordsEqualToInBlocks refs blcs
wordsEqualToInBlock refs (BlockEx exs) = concat $ wordsEqualToInParaPart refs <$> unwrapExamples exs
wordsEqualToInBlock refs (BlockQuotes blcs) = wordsEqualToInBlocks refs blcs
wordsEqualToInBlock refs (Table prts) = wordsEqualToInParaParts refs prts
wordsEqualToInBlock _ _ = []


wordsEqualToInBlocks :: [Word] -> [Block] -> [Inline]
wordsEqualToInBlocks refs blcs = concatMap (wordsEqualToInBlock refs) blcs

wordsEqualToInSection :: [Word] -> Section -> [Inline]
wordsEqualToInSection refs (Section _ _ (Title inls) bdy secs) =
  wordsEqualToInInlines refs inls ++
  wordsEqualToInBlocks refs (secblcs . secbdy $ bdy) ++
  wordsEqualToInSections refs secs
wordsEqualToInSection refs (SecBlocks blcs) = wordsEqualToInBlocks refs blcs
wordsEqualToInSection refs (Meta mp) =
  titleWordsEqualToInlines mp refs ++ abstractWordsEqualToInlines mp refs

--meta fields

titleWordsEqualToInlines :: M.Map MetaKey MetaValue -> [Word] -> [Inline]
titleWordsEqualToInlines mp refs = procTitle mp (wordsEqualToInInlines refs) []

abstractWordsEqualToInlines :: M.Map MetaKey MetaValue -> [Suffix] -> [Inline]
abstractWordsEqualToInlines mp refs = procAbstract mp (wordsWithInBlocks refs) []

wordsEqualToInSections :: [Word] -> [Section] -> [Inline]
wordsEqualToInSections refs secs = concatMap (wordsEqualToInSection refs) secs

wordsEqualToInDocument :: [Word] -> Document -> [Inline]
wordsEqualToInDocument refs (Doc secs) = wordsEqualToInSections refs secs


