{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Attributes where

import Data.List (delete)

import Abac.Types.ParserTypes
--import Abac.Traverse.Searchable

-- Attributes --
----------------

getAttrs :: Inline -> [Attr]
getAttrs inl =
  case inl of
   Word attrs _ _        -> attrs
   Citation attrs _ _ _  -> attrs
   Number attrs _ _      -> attrs
   Email attrs _ _ _     -> attrs
   _                     -> []


-- add attributes etc

addEmph :: Inline -> Inline
addEmph = addAttrToInl Emph

addBold :: Inline -> Inline
addBold = addAttrToInl Bold

addAttrToDoc :: Attr -> Document -> Document
addAttrToDoc attr (Doc scts) = Doc $ addAttrToSections attr scts

addAttrToInl :: Attr -> Inline -> Inline
addAttrToInl attr (Word attrs pos txt)         = Word (updateAttrs attr attrs) pos txt
addAttrToInl attr (Citation attrs int pos txt) = Citation (updateAttrs attr attrs) int pos txt
addAttrToInl attr (Number attrs pos dbl)       = Number (updateAttrs attr attrs) pos dbl
addAttrToInl attr (Email attrs pos t1 t2)      = Email (updateAttrs attr attrs) pos t1 t2
addAttrToInl _ inl                             = inl

updateAttrs :: Attr -> [Attr] -> [Attr]
updateAttrs attr attrs =
  let nonempty = delete None attrs
  in  if attr `elem` attrs then nonempty else (attr : nonempty)

addAttrToExp :: Attr -> Expression -> Expression
addAttrToExp attr (Expression inls) = Expression $ addAttrToInl attr <$> inls

addAttrToParaParts :: Attr -> [ParaPart] -> [ParaPart]
addAttrToParaParts attr prts = addAttrToParaPart attr <$> prts

addAttrToParaPart :: Attr -> ParaPart -> ParaPart
addAttrToParaPart attr (Sentence inls) = Sentence $ addAttrToInl attr <$> inls
addAttrToParaPart attr (Inlines inls) = Inlines $ addAttrToInl attr <$> inls
addAttrToParaPart attr (ParaFtn n snts) = ParaFtn n (addAttrToParaParts attr snts)
addAttrToParaPart attr (Caption prts) = Caption (addAttrToParaParts attr prts)
addAttrToParaPart _ prt = prt

addAttrToBlocks :: Attr -> [Block] -> [Block]
addAttrToBlocks attr blcs = addAttrToBlock attr <$> blcs

addAttrToBlock :: Attr -> Block -> Block
addAttrToBlock attr (Para prts) = Para $ addAttrToParaParts attr prts
addAttrToBlock attr (Footnote n blcs) = Footnote n $ addAttrToBlocks attr blcs
addAttrToBlock attr (BlockEx exs) = BlockEx $ addAttrToEx attr <$> exs
addAttrToBlock attr (BlockQuotes blcs) = BlockQuotes $ addAttrToBlocks attr blcs
addAttrToBlock attr (BlockQuote par) = BlockQuote $ addAttrToBlock attr par
addAttrToBlock _ blc@(BlockTech _ _ _) = blc
addAttrToBlock attr (BlockComment blcs) = BlockComment $ addAttrToBlocks attr blcs
addAttrToBlock _ blc@(LinkRef _ _ _) = blc
addAttrToBlock _ blc@(ImageRef _ _ _) = blc
addAttrToBlock attr (Table prts) = Table $ addAttrToParaParts attr prts

addAttrToEx :: Attr -> Example -> Example
addAttrToEx attr ex@(Example _ _ _ _ _ []) = updateExBdy (addAttrToParaParts attr) ex
addAttrToEx attr ex =
  updateExBdy (addAttrToParaParts attr) . updateSubex (addAttrToEx attr) $ ex


addAttrToSections :: Attr -> [Section] -> [Section]
addAttrToSections attr scts = addAttrToSection attr <$> scts

addAttrToSection :: Attr -> Section -> Section
addAttrToSection _ meta@(Meta _) = meta
addAttrToSection attr (SecBlocks blcs) = SecBlocks $ addAttrToBlocks attr blcs
addAttrToSection attr sct@(Section _ _ _ _ []) = updateSecbdyWith (addAttrToSection attr) sct
addAttrToSection attr sct =
  let f = addAttrToSection attr in updateSecbdyWith f . updateSecsbsWith f $ sct

