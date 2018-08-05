{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Abac.Internal where


import qualified Data.Map.Strict as M
import Text.Megaparsec (SourcePos)
import Text.Megaparsec.Pos (unPos,sourceLine,sourceColumn)
import qualified Data.Text as T (isSuffixOf,empty, append,
                                 intercalate, replicate,
                                 unwords, pack, concat,
                                 cons,snoc,)

import Data.List (find,intersect,isSubsequenceOf)
import Data.Maybe (isJust)
import Prelude hiding (Word)

import Abac.Types.ParserTypes


--converting No to [Int] and back; used for numbering in examples and sections
tupToList :: No -> [Int]
tupToList (i,j,k,l,m,n) = [i,j,k,l,m,n]

listToTup :: [Int] -> No
listToTup [i,j,k,l,m,n] = (i,j,k,l,m,n)
listToTup _ = zeros


--embedding sections


embedAllSections :: [Section] -> [Section]
embedAllSections [] = []
embedAllSections [sec] = [sec]
embedAllSections (sec1 : sec2 : secs)
  | isPureDiv sec1 = sec1 : embedAllSections (sec2 : secs)
  | lev2 > lev1    = embedAllSections (sec1 `embedsSec` sec2 : secs)
  | otherwise      = sec1 : embedAllSections (sec2 : secs) -- section sec1 may be 'stranded'
  where
    lev1 = seclev sec1
    lev2 = seclev sec2

embedsSec :: Section -> Section -> Section
embedsSec sec1 sec2 = go diff sec1 sec2
  where
    diff = seclev sec2 - seclev sec1
    go :: Int -> Section -> Section -> Section
    go 1 (Section no lev ttl bdy subs) sec' = Section no lev ttl bdy (subs ++ [sec'])
    go i (Section no lev ttl bdy subs) sec'
      | null subs = Section no lev ttl bdy (sec' : subs)
      | otherwise = Section no lev ttl bdy (init subs ++ [go (i - 1) (last subs) sec'])
    go _ (SecBlocks _) _ = error "embedsSec: nameless sections must not embed sections"
    go _ (Meta _) _ = error "embedsSec: meta sections must not embed sections"

-- to avoid applying the record selector `seclev` to Section values that do not have it
assignLev :: Section -> Level
assignLev sc =
  case sc of
    Section _ _ _ _ _ -> seclev sc
    _                 -> 0



----------------
-- Predicates --
----------------

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



-- block indices
type ParaIndex = Int

--lists of blocks will be made into maps

mapit :: [a] -> M.Map Int a
mapit xs = M.fromList $ zip [1 ..] xs


-- auxiliary functions used to number sections and examples

-- e.g. turns (1,1,0,0,1,0) into (1,1,1,1,1,0)
fillWith1s :: No -> No
fillWith1s = listToTup . fillListWith1s . tupToList
  where
    fillListWith1s :: [Int] -> [Int]
    fillListWith1s xs =
      let (zeros', onesAndZeros) = span (== 0) (reverse xs)
          zeroToOne x = if x == 0 then 1 else x
      in  reverse $ zeros' ++ fmap zeroToOne onesAndZeros

add :: No -> No -> No
add (i1,j1,k1,l1,m1,n1) (i2,j2,k2,l2,m2,n2) =
  (i1 + i2, j1 + j2, k1+ k2, l1 + l2, m1 + m2, n1 + n2)

levToNos :: Level -> [No]
levToNos lev = levToNos' 1 lev

levToNos' :: Int -> Level -> [No]
levToNos' n lev =
  case lev of
    1 -> [ (i,0,0,0,0,0) | i <- [n ..] ]
    2 -> [ (0,i,0,0,0,0) | i <- [n ..] ]
    3 -> [ (0,0,i,0,0,0) | i <- [n ..] ]
    4 -> [ (0,0,0,i,0,0) | i <- [n ..] ]
    5 -> [ (0,0,0,0,i,0) | i <- [n ..] ]
    _ -> [ (0,0,0,0,0,i) | i <- [n ..] ]


-- positions of parsed items

getLineNumber :: SourcePos -> LineNo
getLineNumber = fromIntegral . unPos . sourceLine

getColumnNumber :: SourcePos -> ColumnNo
getColumnNumber = fromIntegral . unPos . sourceColumn

--Megaparsec gives the column source position of the end of the parsed string, so
--n in `posFromSource` is the offset from the beginning of the parsed string; we
--will take n = length of the parsed string in order to determine the starting
--column number. NB: the +1 (in n+1) is needed because Megaparsec outpus the column
--at the end of the string, including the space following it -- effectively the position
--of the remainder of the string to be parsed (after successfully paring the target string).

posFromSource :: Int -> SourcePos -> Position
posFromSource n p = (getLineNumber p, getColumnNumber p - (n + 1))


--expression length

expressionLength :: Expression -> Int
expressionLength (Expression inls) = length inls

averageExprLength :: [Expression] -> Int
averageExprLength exps = (sum $ map expressionLength exps) `div` length exps

--Function that computes the percentage of a certain kind of words in the total
--number of words

(%>) :: Double -> Double -> Double
(%>) = \ x y -> x * 100 / y

ignored :: Double
ignored = (-1.0) -- elements assigned -1 are ignored

average :: [Double] -> Double
average [] = 0
average dbls' = sum dbls / (fromIntegral . length) dbls
  where dbls = filter (/= ignored) dbls'

--Give me the number of words hosted by the Either's Right, if any

number :: (Num c, Functor f) => f [b] -> f c
number eitherVal = (fromIntegral . length) <$> eitherVal

--a version of isSuffixOf that works over Words

isMySuffixOf :: Suffix -> Word -> Bool
isMySuffixOf (Word _ _ end) (Word _ _ wrd) = end `T.isSuffixOf` wrd
isMySuffixOf _ _ = False

-- more predicates

wordlike :: Inline -> Bool
wordlike inl = isWord inl || isCit inl || isTech inl || isNumber inl || isEmail inl

onlyWords :: [Inline] -> Bool
onlyWords inls = not $ hasNonWords inls

hasNonWords :: [Inline] -> Bool
hasNonWords inls =
  isJust $ find (\x -> isPunct x || isTech x || isNumber x || isCit x || isSpace x) inls


-- overlap between expressions

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

intersectExpressions :: Expression -> Expression -> [Inline]
intersectExpressions (Expression inls1) (Expression inls2) =
  mkWords $ intersect (unwrapInlines inls1) (unwrapInlines inls2)

-- build words
mkWords :: [Text] -> [Word]
mkWords txts = mkWord <$> txts

mkWord :: Text -> Word
mkWord = Word [None] (0,0)

-- expression exp1 has words that appear in exp2 *in the same order*
contains :: Expression -> Expression -> Bool
contains (Expression []) _ = False
contains _ (Expression []) = False
contains (Expression inls1) (Expression inls2) =
  inls2 `isSubsequenceOf` inls1



-------------
-- Textify --
-------------


--decode document into text

lineend, parend, spc :: Text
lineend = T.pack "\n"
parend = T.pack "\n\n"
spc = T.pack " "

unwrapInline :: Inline -> Text
unwrapInline (Word _ _ txt) = txt
unwrapInline (Citation _ _ _ txt) = txt
unwrapInline (Email _ _ nom dom) = T.concat [nom, "@", dom]
unwrapInline (InlineTech _ _ txt) = txt
unwrapInline (Number _ _  dbl) = T.pack $ show dbl
unwrapInline (Punct _ txt) = txt
unwrapInline (Link inls _) = T.unwords $ fmap unwrapInline inls
unwrapInline (Image inls _) = T.unwords $ fmap unwrapInline inls
unwrapInline Space = T.pack " "
unwrapInline Newline = lineend
unwrapInline ParEnd = parend
unwrapInline _ = T.empty

unwrapParaPart :: ParaPart -> Text
unwrapParaPart (Sentence inls) = T.unwords $ fmap unwrapInline inls
unwrapParaPart (ParaFtn _ snts) =
  let footnote = T.intercalate spc $ fmap unwrapParaPart snts
      leftMarker = "^[" :: Text
      rightMarker = "]" :: Text
  in  leftMarker `T.append` footnote `T.append` rightMarker
unwrapParaPart (Inlines inls) = T.unwords $ fmap unwrapInline inls
unwrapParaPart (Caption prts) = T.intercalate spc $ fmap unwrapParaPart prts
unwrapParaPart _ = T.empty


unwrapBlock :: Block -> Text
unwrapBlock (Para prts) = T.concat $ fmap unwrapParaPart prts
unwrapBlock (Footnote _ blcs) = T.intercalate parend $ fmap unwrapBlock blcs
unwrapBlock (BlockEx exs) = T.concat $ unwrapParaPart <$> unwrapExamples exs
unwrapBlock (BlockQuotes blcs) = T.intercalate parend $ fmap unwrapBlock blcs
unwrapBlock (BlockTech Math _ txt) = '$' `T.cons` txt `T.snoc` '$'
unwrapBlock (BlockTech Code _ txt) = '`' `T.cons` txt `T.snoc` '`'
unwrapBlock (LinkRef _ _ url) = url
unwrapBlock (ImageRef _ _ path) = path
unwrapBlock (Table prts) = T.intercalate spc $ fmap unwrapParaPart prts
unwrapBlock _ = T.empty


unwrapSection :: Section -> Text
unwrapSection (SecBlocks blcs) = T.intercalate parend $ fmap unwrapBlock blcs
unwrapSection (Section _ lvl ttl bdy subs) =
  let hash = "#" :: Text
      unwrapTitle (Title inls) = T.unwords $ fmap unwrapInline inls
      title = T.replicate lvl hash `T.append` spc `T.append` unwrapTitle ttl `T.append` parend
      body = unwrapSection bdy
      blocks = T.intercalate parend $ fmap unwrapSection subs
  in  title `T.append` body `T.append` blocks
unwrapSection (Meta mp) = T.concat $ addYamlDelimiters $ yamlEntries mp
  where
    addYamlDelimiters :: [Text] -> [Text]
    addYamlDelimiters txts = [delim] ++ txts ++ [delim] where delim = ("---\n" :: Text)

    yamlEntries :: M.Map MetaKey MetaValue -> [Text]
    yamlEntries m = M.elems $ M.mapWithKey makeYamlEntry m
    makeYamlEntry var val = varToString var `T.append` valToString val `T.append` lineend

    varToString :: MetaKey -> Text
    varToString v =
      case v of
        AuthorKey    -> "author: " :: Text
        TitleKey     -> "title: " :: Text
        DateKey      -> "date: " :: Text
        AbstractKey  -> "abstract: " :: Text
        TagKey       -> "tags: " :: Text
        OtherKey txt -> txt :: Text

    valToString :: MetaValue -> Text
    valToString v =
      case v of
        MetaSeq inlss    -> (T.unwords . fmap T.unwords) $ (fmap . fmap) unwrapInline inlss
        MetaNum ints     -> T.concat $ fmap (T.pack . show) ints
        MetaInlines inls -> T.unwords $ fmap unwrapInline inls
        MetaBool bool    -> T.pack $ show bool
        MetaBlocks blcs  -> T.intercalate parend $ fmap unwrapBlock blcs

unwrapDocument :: Document -> Text
unwrapDocument (Doc secs) = T.intercalate spc $ fmap unwrapSection secs

--NB: by unwraping the inlines and then wraping them in Word, we lose
--the distinction between different types of inlines (words, citations
--numbers etc), but for our use cases (counting intersections or string
--rendering) that's ok.

unwrapInlines :: [Inline] -> [Text] -- used for displaying words rather than all inlines
unwrapInlines [] = []
unwrapInlines inls = fmap unwrapInline $ filter wordlike inls


unwrapExpr :: Expression -> [Inline]
unwrapExpr (Expression inls) = inls

wrapInExpr :: [Inline] -> Expression
wrapInExpr inls = Expression inls

-- some unwrapping

textifyInlines :: [Inline] -> Text
textifyInlines [] = T.pack "nothing found"
textifyInlines inls = T.unwords $ unwrapInlines inls

textifyExpr :: Expression -> Text
textifyExpr exps = T.unwords $ unwrapInlines $ unwrapExpr exps

textifySent :: Sentence -> Text
textifySent (Sentence inls) = textifyInlines inls
textifySent _ = T.empty

--unwrap type one level down

unwrapExamples :: [Example] -> [ParaPart]
unwrapExamples [] = []
unwrapExamples exs = concatMap unwrapExample exs

unwrapExample :: Example -> [ParaPart]
unwrapExample (Example _ _ _ _ bdy []) = exbdy bdy
unwrapExample (Example _ _ _ _ _ exs) = concatMap unwrapExample exs


