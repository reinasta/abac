module Abac.Traverse.Select where

import qualified Data.Map.Strict as M
import Prelude hiding (Word)



import Abac.Types.ParserTypes
--import Abac.Internal

--import Abac.Traverse.Searchable

import Abac.Traverse.Internal

--formerly selectWith and selectWithNeg, which count occurences of words satisfying `\x -> op x refs`

--NB: all these functions output inlines, viz. the inlines selected from the given unit
--(e.g. block, sentence etc.). So e.g. selectSentenceWith is shorthand for select a sentence
--with such and such features and give me the inlines in virtue of which it has such features.

selectInlinesWith :: (Inline -> Inline -> Bool) -> [Inline] -> [Inline] -> [Inline]
selectInlinesWith op refs inls = [ inl | inl <- removeInlineWrapers inls, any (`op` inl) refs ]

selectInlinesWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> [Inline] -> [Inline]
selectInlinesWithNeg op refs inls = [ inl | inl <- removeInlineWrapers inls, all (`op` inl) refs ]

selectSentenceWith :: (Inline -> Inline -> Bool) -> [Inline] -> Sentence -> [Inline]
selectSentenceWith op refs (Sentence inls) = selectInlinesWith op refs inls
selectSentenceWith _ _ _ = []

selectSentenceWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> Sentence -> [Inline]
selectSentenceWithNeg op refs (Sentence inls) = selectInlinesWithNeg op refs inls
selectSentenceWithNeg _ _ _ = []

selectSentencesWith :: (Inline -> Inline -> Bool) -> [Inline] -> [Sentence] -> [Inline]
selectSentencesWith op refs snts = concatMap (selectSentenceWith op refs) snts

selectSentencesWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> [Sentence] -> [Inline]
selectSentencesWithNeg op refs snts = concatMap (selectSentenceWithNeg op refs) snts

selectParaWith :: (Inline -> Inline -> Bool) -> [Inline] -> Paragraph -> [Inline]
selectParaWith op refs (Para prts) = selectParaPartsWith op refs prts
selectParaWith _ _ _ = []

selectParaWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> Paragraph -> [Inline]
selectParaWithNeg op refs (Para prts) = selectParaPartsWithNeg op refs prts
selectParaWithNeg _ _ _ = []

selectParaPartsWith :: (Inline -> Inline -> Bool) -> [Inline] -> [ParaPart] -> [Inline]
selectParaPartsWith op refs prts = concatMap (selectParaPartWith op refs) prts

selectParaPartsWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> [ParaPart] -> [Inline]
selectParaPartsWithNeg op refs prts = concatMap (selectParaPartWithNeg op refs) prts

selectParaPartWith :: (Inline -> Inline -> Bool) -> [Inline] -> ParaPart -> [Inline]
selectParaPartWith op refs (Sentence inls) = selectInlinesWith op refs inls
selectParaPartWith op refs (ParaFtn _ snts) = selectSentencesWith op refs snts
selectParaPartWith op refs (Inlines inls) = selectInlinesWith op refs inls
selectParaPartWith op refs (Caption parprts) = selectParaPartsWith op refs parprts
selectParaPartWith _ _ _ = []

selectParaPartWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> ParaPart -> [Inline]
selectParaPartWithNeg op refs (Sentence inls) = selectInlinesWithNeg op refs inls
selectParaPartWithNeg op refs (ParaFtn _ snts) = selectSentencesWithNeg op refs snts
selectParaPartWithNeg op refs (Inlines inls) = selectInlinesWithNeg op refs inls
selectParaPartWithNeg op refs (Caption parprts) = selectParaPartsWithNeg op refs parprts
selectParaPartWithNeg _ _ _ = []

-- selectExampleWith :: (Inline -> Inline -> Bool) -> [Inline] -> Example -> [Inline]
-- selectExampleWith op refs (MainEx _ _ _ snts) = selectSentencesWith op refs snts
-- selectExampleWith op refs (SubEx _ _ _ snts) = selectSentencesWith op refs snts
--
-- selectExampleWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> Example -> [Inline]
-- selectExampleWithNeg op refs (MainEx _ _ _ snts) = selectSentencesWithNeg op refs snts
-- selectExampleWithNeg op refs (SubEx _ _ _ snts) = selectSentencesWithNeg op refs snts
--
-- selectExamplesWith :: (Inline -> Inline -> Bool) -> [Inline] -> [Example] -> [Inline]
-- selectExamplesWith op refs exs = concatMap (selectExampleWith op refs) exs
--
-- selectExamplesWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> [Example] -> [Inline]
-- selectExamplesWithNeg op refs exs = concatMap (selectExampleWithNeg op refs) exs

selectBlockWith :: (Inline -> Inline -> Bool) -> [Inline] -> Block -> [Inline]
selectBlockWith op refs par@(Para _) = selectParaWith op refs par
selectBlockWith op refs (Footnote _ blcs) = selectBlocksWith op refs blcs
selectBlockWith op refs (BlockEx exs) = concat $ selectParaPartWith op refs <$> unwrapExamples exs
selectBlockWith op refs (BlockQuotes blcs) = selectBlocksWith op refs blcs
selectBlockWith _ _ _ = []

selectBlockWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> Block -> [Inline]
selectBlockWithNeg op refs par@(Para _) = selectParaWithNeg op refs par
selectBlockWithNeg op refs (Footnote _ blcs) = selectBlocksWithNeg op refs blcs
selectBlockWithNeg op refs (BlockEx exs) =
  concat $ selectParaPartWithNeg op refs <$> unwrapExamples exs
selectBlockWithNeg op refs (BlockQuotes blcs) = selectBlocksWithNeg op refs blcs
selectBlockWithNeg _ _ _ = []

selectBlocksWith :: (Inline -> Inline -> Bool) -> [Inline] -> [Block] -> [Inline]
selectBlocksWith op refs blcs = concatMap (selectBlockWith op refs) blcs

selectBlocksWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> [Block] -> [Inline]
selectBlocksWithNeg op refs blcs = concatMap (selectBlockWithNeg op refs) blcs

selectSectionWith :: (Inline -> Inline -> Bool) -> [Inline] -> Section -> [Inline]
selectSectionWith op refs (Section _ _ (Title inls) bdy secs) =
  selectInlinesWith op refs inls ++
  selectBlocksWith op refs (secblcs . secbdy $ bdy) ++
  selectSectionsWith op refs secs
selectSectionWith op refs (SecBlocks blcs) = selectBlocksWith op refs blcs
selectSectionWith op refs (Meta mp) = titleSelectInlines mp op refs ++ abstractSelectInlines mp op refs

--meta fields

titleSelectInlines :: M.Map MetaKey MetaValue -> (Inline -> Inline -> Bool) -> [Inline] -> [Inline]
titleSelectInlines mp op refs = procTitle mp (selectInlinesWith op refs) []

abstractSelectInlines :: M.Map MetaKey MetaValue -> (Inline -> Inline -> Bool) -> [Inline] -> [Inline]
abstractSelectInlines mp op refs = procAbstract mp (selectBlocksWith op refs) []

selectSectionWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> Section -> [Inline]
selectSectionWithNeg op refs (Section _ _ (Title inls) bdy secs) =
  selectInlinesWithNeg op refs inls ++
  selectBlocksWithNeg op refs (secblcs . secbdy $ bdy) ++
  selectSectionsWithNeg op refs secs
selectSectionWithNeg op refs (SecBlocks blcs) = selectBlocksWithNeg op refs blcs
selectSectionWithNeg op refs (Meta mp) = titleInlines mp op refs ++ abstractInlines mp op refs

--meta fields

titleInlines :: M.Map MetaKey MetaValue -> (Inline -> Inline -> Bool) -> [Inline] -> [Inline]
titleInlines mp op refs = procTitle mp (selectInlinesWithNeg op refs) []

abstractInlines :: M.Map MetaKey MetaValue -> (Inline -> Inline -> Bool) -> [Inline] -> [Inline]
abstractInlines mp op refs = procAbstract mp (selectBlocksWithNeg op refs) []

selectSectionsWith :: (Inline -> Inline -> Bool) -> [Inline] -> [Section] -> [Inline]
selectSectionsWith op inls secs = concatMap (selectSectionWith op inls) secs

selectSectionsWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> [Section] -> [Inline]
selectSectionsWithNeg op inls secs = concatMap (selectSectionWithNeg op inls) secs

selectDocumentWith :: (Inline -> Inline -> Bool) -> [Inline] -> Document -> [Inline]
selectDocumentWith op inls (Doc secs) = selectSectionsWith op inls secs

selectDocumentWithNeg :: (Inline -> Inline -> Bool) -> [Inline] -> Document -> [Inline]
selectDocumentWithNeg op inls (Doc secs) = selectSectionsWith op inls secs


