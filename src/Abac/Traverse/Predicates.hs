module Abac.Traverse.Predicates where


import Data.Maybe (isJust)
import Data.List (find)
import Prelude hiding (Word)

import Abac.Types.ParserTypes
--import Abac.Internal

--inline level predicates

isWord :: Inline -> Bool
isWord (Word _ _ _) = True
isWord _              = False

isCit :: Inline -> Bool
isCit (Citation _ _ _ _) = True
isCit _                  = False

isInlineComment :: Inline -> Bool
isInlineComment (InlineComment _) = True
isInlineComment _ = False

isEmail :: Email -> Bool
isEmail (Email _ _ _ _) = True
isEmail _               = False

isTech :: Inline -> Bool
isTech (InlineTech _ _ _) = True
isTech _                    = False

isMath :: Inline -> Bool
isMath (InlineTech Math _ _) = True
isMath _                       = False

isCode :: Inline -> Bool
isCode (InlineTech Code _ _) = True
isCode _                       = False

isPunct :: Inline -> Bool
isPunct (Punct _ _) = True
isPunct _               = False

isPunctEs :: Inline -> Bool
isPunctEs (Punct EndSentence _) = True
isPunctEs _                         = False

isPunctIs :: Inline -> Bool
isPunctIs (Punct InSentence _) = True
isPunctIs _                        = False

isNumber :: Inline -> Bool
isNumber (Number _ _ _) = True
isNumber _              = False

isLink :: Inline -> Bool
isLink (Link _ _) = True
isLink _ = False

isImage :: Inline -> Bool
isImage (Image _ _) = True
isImage _ = False

isSentFtn :: Inline -> Bool
isSentFtn (SentFtn _ _) = True
isSentFtn _ = False

isLinkRef :: Block -> Bool
isLinkRef (LinkRef _ _ _) = True
isLinkRef _ = False

isImageRef :: Block -> Bool
isImageRef (ImageRef _ _ _) = True
isImageRef _ = False

isSpace :: Inline -> Bool
isSpace Space = True
isSpace _     = False

isNewline :: Inline -> Bool
isNewline Newline = True
isNewline _ = False

isParEnd :: Inline -> Bool
isParEnd ParEnd = True
isParEnd _ = False

isNull :: Inline -> Bool
isNull Null = True
isNull _ = False

--markers

isExMarker :: Marker -> Bool
isExMarker (ExMark _ _ _) = True
isExMarker _ = False

isItMarker :: Marker -> Bool
isItMarker (ItMark _ _ _) = True
isItMarker _ = False

isFtnMarker :: Marker -> Bool
isFtnMarker (FtnMark _ _) = True
isFtnMarker _ = False

isRefExMarker :: Marker -> Bool
isRefExMarker (RefEx _ _) = True
isRefExMarker _ = False

isRefFtnMarker :: Marker -> Bool
isRefFtnMarker (RefFtn _ _) = True
isRefFtnMarker _ = False

--sentence (ParaPart) level predicates

isSentence :: ParaPart -> Bool
isSentence (Sentence _) = True
isSentence _ = False

isFootnoteS :: ParaPart -> Bool
isFootnoteS (ParaFtn _ _) = True
isFootnoteS _ = False

isPureInlines :: ParaPart -> Bool
isPureInlines (Inlines _) = True
isPureInlines _ = False

isParaComment :: ParaPart -> Bool
isParaComment (ParaComment _) = True
isParaComment _ = False

isCaption :: ParaPart -> Bool
isCaption (Caption _) = True
isCaption _ = False

isParaPart :: ParaPart -> Bool
isParaPart prt = isSentence prt || isFootnoteS prt || isPureInlines prt

--block level predicates and above

isSection :: Section -> Bool
isSection (Section _ _ _ _ _) = True
isSection _ = False

isPureDiv :: Section -> Bool
isPureDiv (SecBlocks _) = True
isPureDiv _ = False

isMeta :: Section -> Bool
isMeta (Meta _) = True
isMeta _ = False

isParagraph :: Block -> Bool
isParagraph (Para _) = True
isParagraph _ = False

isFootnoteB :: Block -> Bool
isFootnoteB (Footnote _ _) = True
isFootnoteB _ = False

isBlockEx :: Block -> Bool
isBlockEx (BlockEx _) = True
isBlockEx _ = False

isOrdered :: Example -> Bool
isOrdered (Example Ordered _ _ _ _ _) = True
isOrdered _ = False

isUnordered :: Example -> Bool
isUnordered (Example Unordered _ _ _ _ _) = True
isUnordered _ = False

isQuote :: Block -> Bool
isQuote (BlockQuotes _) = True
isQuote _ = False

isBlockTech :: Block -> Bool
isBlockTech (BlockTech _ _ _) = True
isBlockTech _ = False

isTable :: Block -> Bool
isTable (Table _) = True
isTable _ = False

isBlockComment :: Block -> Bool
isBlockComment (BlockComment _) = True
isBlockComment _ = False

isBlock :: Block -> Bool
isBlock x = isParagraph x || isFootnoteB x || isBlockEx x || isBlockComment x ||
  isQuote x || isBlockTech x || isLinkRef x || isImageRef x || isTable x

-- words, pure words etc.

wordlike :: Inline -> Bool
wordlike inl = isWord inl || isCit inl || isTech inl || isNumber inl || isEmail inl

onlyWords :: [Inline] -> Bool
onlyWords inls = not $ hasNonWords inls

hasNonWords :: [Inline] -> Bool
hasNonWords inls =
  isJust $ find (\x -> isPunct x || isTech x || isNumber x || isCit x || isSpace x) inls



--handling styling and other attributes

isEmph :: Inline -> Bool
isEmph (Word attrs _ _) | Emph `elem` attrs = True
                        | otherwise         = False
isEmph _ = False

isBold :: Inline -> Bool
isBold (Word attrs _ _) | Bold `elem` attrs = True
                        | otherwise             = False
isBold _ = False

-- styling filters on inlines

emph :: [Inline] -> [Inline]
emph inls = filter isEmph inls

bold :: [Inline] -> [Inline]
bold inls = filter isBold inls

