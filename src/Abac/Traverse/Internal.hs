{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Abac.Traverse.Internal where

import qualified Data.Text as T (Text, splitOn, length,
                                 append, intercalate, toTitle,
                                 pack, unpack, count, concat)


import Data.Maybe (isJust)
import Data.List
import Data.Char (intToDigit)
import Control.Error.Util (note)
import Control.Applicative
import qualified Data.Map.Strict as M
import Prelude hiding (Word)

import Abac.Types.ParserTypes
import Abac.Internal


--A class for AST elements, Inline, Sentence, Block, Section, Doc
class Element a

instance Element Inline
instance Element Expression
instance Element ParaPart
instance Element Block
instance Element Section
instance Element Document


----------------
-- Searchable --
----------------

-- | Terminology: an /element/ is a node in the Abac tree such as a section, a block,
-- or a sentence. A /word/ is any inline except for spaces and punctuation, that is,
-- a citation, a number, an email address, a tech (math or code) inline, or a genuine
-- word.

class Searchable a where

  -- Search based on word equality and constitution
  wordsWith              :: [Suffix] -- ^ list of suffixes to use for search
                         -> a        -- ^ doc element to search in
                         -> [Inline] -- ^ words with previous suffixes
  -- | @wordsWith sufs el@ searches the element @el@ all words (inlines) with one of the
  -- suffixes in @sufs@

  wordsEqualTo           :: [Word] -- ^ list of words to look for
                         -> a      -- ^ doc element to search in
                         -> [Word] -- ^ words found
  -- | @wordsEqualTo wds el@ searches the document element @el@ for words equal to any of
  -- the words in @wds@
  --
  -- Note that word equality is equality on the character strings wrapped in the 'Inline' type,
  -- e.g. @Word [Bold] (1,1) "blah" == Word [Emph] (5,4) "blah"@

  expressionsWith        :: Int          -- ^ the number of inlines in expression
                         -> [Word]       -- ^ list of words to look for in each expression
                         -> a            -- ^ doc element to search in
                         -> [Expression] -- ^ expressions found
  -- | @expressionsWith n wds el@ searches the element @el@ for expressions of n inlines that
  -- contain any of the words @wds@ among its constituents


  expressionsEqualTo     :: [Expression] -- ^ expressions to look for
                         -> a            -- ^ element to be searched
                         -> [Expression] -- ^ expressions found
  -- | @expressionsEqualTo exps el@ searches the element @el@ for expressions equal to any of
  -- the expressions in @exps@.
  --
  -- Two expressions are equal just in case their inline constituents are equal


  countWordsWith         :: [Suffix] -> a -> Int
  -- | @countWordsWith sufs el == length $ wordsWith sufs el@

  proportionWordsWith    :: [Suffix] -> a -> Double
  -- | @proportionWordsWith sufs el@ returns the percentage of words ending with any of the
  -- @sufs@ relative to the total number of words in @el@

  countWordsEqualTo      :: [Word] -> a -> Int
  -- | @countWordsEqualTo wds el == length $ wordsEqualTo wds el@

  proportionWordsEqualTo :: [Word] -> a -> Double
  -- | @proportionWordsEqualTo wds el@ returns the percentage of words equal to any of the
  -- @wds@ relative to the element @el@'s total number of words

  countExpressions       :: [Expression] -> a -> Int
  -- | @countExpressions exps el == length $ expressionsEqualTo exps el@

  proportionExpressions  :: [Expression] -> a -> Double
  -- | @proportionExpressions exps el@ is the percentage of expressions equal to any of @exps@
  -- relative to the total number of expressions in el.

  -- The total number of expressions in @el@ is somewhat arbitrarily calculated as the number
  -- of contiguous (non-overlapping) ngrams in @el@, where n (of *n*gram) is the number of words
  -- in the head of @exps@ (if @exps@ is non-empty; if empty, the percentage is taken to be 0.)

  -- Extract tree elements from larger tree elements
  ngramsIn               :: Int -> a -> [Expression]
  -- | @ngramsIn n el@ produces the list of ngrams in @el@; the number of words in each ngram
  -- is given by @n@.

  gatherInlines          :: a -> [Inline]
  -- | collects the inlines in a given element

  gatherParaParts        :: a -> [ParaPart]
  -- | collects the paragraph parts (values of 'ParaPart', e.g. sentences) in a given element

  gatherBlocks           :: a -> [Block]
  -- | collects the blocks in a given element

  gatherSections         :: a -> [Section]
  -- | collects the sections in a given element

  getPosition            :: a -> (Position,Position)
  -- | this takes an element @el@ and returns the position of the first and last inline of @el@
  -- in a tuple

  gatherExamples :: a -> [Example]
  gatherExamples el =
    let exblcs = filter isBlockEx $ gatherBlocks el
        unwrapExBlcs (BlockEx exs) = exs
        unwrapExBlcs _ = []
    in  concatMap unwrapExBlcs exblcs
  -- | collects the example items in a given element



  -- Produce word list and count
  wordsCommon            :: a -> [Inline]
  wordsCommon el = filter wordlike $ gatherInlines el
  -- | lists all the words in an element

  countWordsIn           :: a -> Int
  countWordsIn el = length $ wordsCommon el
  -- | counts all the words in an element


  -- Attribute updates and attribute-based searches

  addAttr                :: Attr -> a -> a
  -- | a function that adds the given attribute to an element.
  --
  -- For an element to have an attribute is for all the (attribute-bearing) inlines
  -- of that element to have that attribute.

  hasAttrs               :: a -> [Attr] -> Bool
  hasAttrs = hasAttrs'
  -- | @hasAttrs el attrs@ is true just in case the element @el@ has all of the
  -- attributes in @attrs@

  hasExactlyAttrs        :: a -> [Attr] -> Bool
  hasExactlyAttrs = hasExactlyAttrs'
  -- | a strict version of @hasAttrs@; @hasExactlyAttrs el attrs@ returns true just
  -- in case the element @el@ has all the attributes in @attrs@

  someHaveAttrs          :: a -> [Attr] -> Bool
  someHaveAttrs = someHaveAttrs'
  -- | @someHaveAttrs el attrs@ returns true if some inlines within @el@ have the
  -- attributes in attrs. Check that this is correct!

  hasSomeAttrs           :: a -> [Attr] -> Bool
  hasSomeAttrs = hasSomeAttrs'
  -- | this function returns true if some inlines in the given element have some of
  -- the given attributes. Check that this is correct!

  -- Traverse the document tree, collecting inlines and expressions

  traverseWith           :: (Inline -> Bool) -> a -> [Inline]
  traverseWith predic el = filter predic $ gatherInlines el
  -- | an inline filter on the given element

  traverseWith'           :: Int -> (Expression -> Bool) -> a -> [Expression]
  traverseWith' n predic el = filter predic $ ngramsIn n el
  -- | an expression filter on the given element


instance Searchable Inline where
  ngramsIn _ _ = []
  wordsWith sffxs inl = [ inl | sffx <- sffxs, sffx `isMySuffixOf` inl ]
  wordsEqualTo wrds inl = [ wrd | wrd <- wrds, wrd == inl ]
  expressionsWith _ _ _ = []
  expressionsEqualTo _ _ = []
  countWordsWith sffxs inl = length $ wordsWith sffxs inl
  proportionWordsWith sffxs inl = fromIntegral $ countWordsWith sffxs inl
  countWordsEqualTo wrds inl = length $ wordsEqualTo wrds inl
  proportionWordsEqualTo wrds inl = fromIntegral $ countWordsEqualTo wrds inl
  countExpressions _ _ = 0
  proportionExpressions _ _ = 0
  gatherInlines inl = [inl]
  gatherParaParts _ = []
  gatherBlocks _ = []
  gatherSections _ = []
  getPosition inl = let pos = inlinePos inl in (pos,pos)
  addAttr = addAttrToInl

instance Searchable Expression where
  ngramsIn n (Expression inls) = ngramsInInlines' n inls
  wordsWith sffxs (Expression inls) = wordsWithInInlines sffxs inls
  wordsEqualTo wrds (Expression inls) = wordsEqualToInInlines wrds inls
  expressionsWith n wrds (Expression inls) = expressionsWithInParaPart n wrds (Inlines inls)
  expressionsEqualTo exps (Expression inls) = expressionsEqualToInInlines exps inls
  countWordsWith sffxs (Expression inls) = targetWordsInlines sffxs inls
  proportionWordsWith sffxs (Expression inls) = proportionWordsInlines sffxs inls
  countWordsEqualTo wrds (Expression inls) = targetEqWordsInlines wrds inls
  proportionWordsEqualTo wrds (Expression inls) = proportionEqWordsInlines wrds inls
  countExpressions exps (Expression inls) = targetExpressionsInlines exps inls
  proportionExpressions exps (Expression inls) = proportionExpressionsInlines exps inls
  gatherInlines (Expression inls) = inls
  gatherParaParts _ = []
  gatherBlocks _ = []
  gatherSections _ = []
  getPosition = expressionPos
  addAttr = addAttrToExp

instance Searchable ParaPart where
  ngramsIn = ngramsInParaPartFlat
  wordsWith = wordsWithInParaPart
  wordsEqualTo = wordsEqualToInParaPart
  expressionsWith = expressionsWithInParaPart
  expressionsEqualTo = expressionsEqualToInParaPart
  countWordsWith = targetWordsParaPart
  proportionWordsWith = proportionWordsParaPart
  countWordsEqualTo = targetEqWordsParaPart
  proportionWordsEqualTo = proportionEqWordsParaPart
  countExpressions = targetExpressionsParaPart
  proportionExpressions = proportionExpressionsParaPart
  gatherInlines = unwrapParaPart'
  gatherParaParts prt = [prt]
  gatherBlocks _ = []
  gatherSections _ = []
  getPosition = paraPartPos
  addAttr = addAttrToParaPart

instance Searchable Block where
  ngramsIn = ngramsInBlockFlat
  wordsWith = wordsWithInBlock
  wordsEqualTo = wordsEqualToInBlock
  expressionsWith = expressionsWithInBlock
  expressionsEqualTo = expressionsEqualToInBlock
  countWordsWith = targetWordsBlock
  proportionWordsWith = proportionWordsBlock
  countWordsEqualTo = targetEqWordsBlock
  proportionWordsEqualTo = proportionEqWordsBlock
  countExpressions = targetExpressionsBlock
  proportionExpressions = proportionExpressionsBlock
  gatherInlines = blockToInlines
  gatherParaParts = blockToParaParts
  gatherBlocks blc = [blc]
  gatherSections _ = []
  getPosition = blockPos
  addAttr = addAttrToBlock

instance Searchable Section where
  ngramsIn = ngramsInSection
  wordsWith = wordsWithInSection
  wordsEqualTo = wordsEqualToInSection
  expressionsWith = expressionsWithInSection
  expressionsEqualTo = expressionsEqualToInSection
  countWordsWith = targetWordsSection
  proportionWordsWith = proportionWordsSection
  countWordsEqualTo = targetEqWordsSection
  proportionWordsEqualTo = proportionEqWordsSection
  countExpressions = targetExpressionsSection
  proportionExpressions = proportionExpressionsSection
  gatherInlines = getInlinesFromDiv
  gatherParaParts = getParaPartsFromDiv
  gatherBlocks = unwrapSection'
  gatherSections sec = [sec]
  getPosition = divisionPos
  addAttr = addAttrToSection

instance Searchable Document where
  ngramsIn = ngramsInDocument
  wordsWith = wordsWithInDocument
  wordsEqualTo = wordsEqualToInDocument
  expressionsWith = expressionsWithInDocument
  expressionsEqualTo = expressionsEqualToInDocument
  countWordsWith = targetWordsDocument
  proportionWordsWith = proportionWordsDocument
  countWordsEqualTo = targetEqWordsDocument
  proportionWordsEqualTo = proportionEqWordsDocument
  countExpressions = targetExpressionsDocument
  proportionExpressions = proportionExpressionsDocument
  gatherInlines = getInlines
  gatherParaParts = getParaParts
  gatherBlocks = getBlocks
  gatherSections = getSections
  getPosition = documentPos
  addAttr = addAttrToDoc


------------
-- Ngrams --
------------

-- We remove the Link and Image wrapers (which wrap a list of inlines) and
-- add the wrapped inlines to the initial list of inlines (the argument).

removeInlineWrapers :: [Inline] -> [Inline]
removeInlineWrapers [] = []
removeInlineWrapers (inl:inls) =
  case inl of
    (Link inls' _)  -> inls' ++ removeInlineWrapers inls
    (Image inls' _) -> inls' ++ removeInlineWrapers inls
    _               -> inl : removeInlineWrapers inls


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

ngramsInDocument :: Int -> Document -> [Expression]
ngramsInDocument n (Doc secs) = concatMap (ngramsInSection n) secs


-----------------
-- Expressions --
-----------------

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


--noOverlapOf' 2 prepositions <$> expressionsWith m prepositions <$>
--   ngramsInBlockFlat n <$> mapit (getBlocks doc)


-----------
-- Count --
-----------

--versions of countElemOf

countElemsOfInInlines :: [Word] -> [Inline] -> Int
countElemsOfInInlines ws inls = sum [1 | w <- ws, any (== w) (removeInlineWrapers inls)]

-- countElemsOfInInlines ws (inl:inls) =
--   case inl of
--     (Link inls' _)   -> countElemsOfInInlines ws inls' + countElemsOfInInlines ws inls
--     (Image inls' _)  -> countElemsOfInInlines ws inls' + countElemsOfInInlines ws inls
--     _                -> sum [1 | w <- ws, w == inl] + countElemsOfInInlines ws inls


countElemsOfInExpr :: [Word] -> Expression -> Int
countElemsOfInExpr ws (Expression inls) = countElemsOfInInlines ws inls

countElemsOfInSent :: [Word] -> Sentence -> Int
countElemsOfInSent ws (Sentence inls) = countElemsOfInInlines ws inls
countElemsOfInSent _ _ = 0

countElemsOfInSents :: [Word] -> [Sentence] -> Int
countElemsOfInSents ws snts = sum $ countElemsOfInSent ws <$> snts

-- countElemsOfInExample :: [Word] -> Example -> Int
-- countElemsOfInExample ws (MainEx _ _ _ snts) = sum $ countElemsOfInSent ws <$> snts
-- countElemsOfInExample ws (SubEx _ _ _ snts) = sum $ countElemsOfInSent ws <$> snts
--
-- countElemsOfInExamples :: [Word] -> [Example] -> Int
-- countElemsOfInExamples ws exs = sum $ countElemsOfInExample ws <$> exs

countElemsOfInParaPart :: [Word] -> ParaPart -> Int
countElemsOfInParaPart ws (ParaFtn _ snts)  = countElemsOfInSents ws snts
countElemsOfInParaPart ws (Caption parprts) = sum $ countElemsOfInParaPart ws <$> parprts
countElemsOfInParaPart ws snt@(Sentence _) = countElemsOfInSent ws snt
countElemsOfInParaPart _ _ = 0

countElemsOfInPara :: [Word] -> Paragraph -> Int
countElemsOfInPara ws (Para prts) = sum $ countElemsOfInParaPart ws <$> prts
countElemsOfInPara _ _ = 0

countElemsOfInBlock :: [Word] -> Block -> Int
countElemsOfInBlock ws par@(Para _) = countElemsOfInPara ws par
countElemsOfInBlock ws (Footnote _ blcs) = sum $ countElemsOfInBlock ws <$> blcs
countElemsOfInBlock ws (BlockEx exs) = sum $ countElemsOfInParaPart ws <$> unwrapExamples exs
countElemsOfInBlock ws (BlockQuotes blcs) = sum $ countElemsOfInBlock ws <$> blcs
countElemsOfInBlock ws (BlockQuote par) = countElemsOfInPara ws par
countElemsOfInBlock ws (Table prts) = sum $ countElemsOfInParaPart ws <$> prts
countElemsOfInBlock _ (BlockTech _ _ _) = 0
countElemsOfInBlock _ (LinkRef _ _ _) = 0
countElemsOfInBlock _ (ImageRef _ _ _) = 0
countElemsOfInBlock _ (BlockComment _) = 0

countElemsOfInBlocks :: [Word] -> [Block] -> Int
countElemsOfInBlocks ws blcs = sum $ countElemsOfInBlock ws <$> blcs

countElemsOfInSection :: [Word] -> Section -> Int
countElemsOfInSection ws (Section _ _ (Title inls) bdy subs) =
  countElemsOfInInlines ws inls +
  countElemsOfInBlocks ws (secblcs . secbdy $ bdy) +
  countElemsOfInSections ws subs
countElemsOfInSection ws (SecBlocks blcs) = countElemsOfInBlocks ws blcs
countElemsOfInSection ws (Meta mp) = titleCount ws mp + abstractCount ws mp + tagCount ws mp

--meta fields

titleCount :: [Word] -> M.Map MetaKey MetaValue -> Int
titleCount ws mp = procTitle mp (countElemsOfInInlines ws) 0

abstractCount :: [Word] -> M.Map MetaKey MetaValue -> Int
abstractCount ws mp = procAbstract mp (countElemsOfInBlocks ws) 0

tagCount :: [Word] -> M.Map MetaKey MetaValue -> Int
tagCount ws mp = procTags mp (countElemsOfInInlines ws) 0

countElemsOfInSections :: [Word] -> [Section] -> Int
countElemsOfInSections ws secs = sum $ countElemsOfInSection ws <$> secs

countElemsOfInDocument :: [Word] -> Document -> Int
countElemsOfInDocument ws (Doc secs) = countElemsOfInSections ws secs

--formerly quantifyWordsIn... functions

countWordsInInlines :: [Inline] -> Int
countWordsInInlines = length . wordlikeInInlines . removeInlineWrapers

countWordsInSent :: Sentence -> Int
countWordsInSent (Sentence inls) = countWordsInInlines inls
countWordsInSent _ = 0

countWordsInSents :: [Sentence] -> Int
countWordsInSents snts = sum $ countWordsInSent <$> snts

-- countWordsInExample :: Example -> Int
-- countWordsInExample (MainEx _ _ _ snts) = countWordsInSents snts
-- countWordsInExample (SubEx _ _ _ snts) = countWordsInSents snts
--
-- countWordsInExamples :: [Example] -> Int
-- countWordsInExamples exs = sum $ countWordsInExample <$> exs

countWordsInParaParts :: ParaPart -> Int
countWordsInParaParts snt@(Sentence _) = countWordsInSent snt
countWordsInParaParts (ParaFtn _ snts) = countWordsInSents snts
countWordsInParaParts (Inlines inls) = countWordsInInlines inls
countWordsInParaParts (Caption prts) = sum $ countWordsInParaParts <$> prts
countWordsInParaParts _ = 0

-- this should take the name of the function above, which should be in turn renamed in the singular
countWordsInParaParts' :: [ParaPart] -> Int
countWordsInParaParts' prts = sum $ countWordsInParaParts <$> prts


countWordsInPar :: Paragraph -> Int
countWordsInPar (Para prts) = sum $ countWordsInParaParts <$> prts
countWordsInPar _ = 0

countWordsInBlock :: Block -> Int
countWordsInBlock par@(Para _) = countWordsInPar par
countWordsInBlock (Footnote _ blcs) = countWordsInBlocks blcs
countWordsInBlock (BlockEx exs) = sum $ countWordsInParaParts <$> unwrapExamples exs
countWordsInBlock (LinkRef _ _ _) = 1
countWordsInBlock (ImageRef _ _ _) = 1
countWordsInBlock (BlockQuote par) = countWordsInPar par
countWordsInBlock (BlockComment _) = 0
countWordsInBlock (Table parprts) = sum $ countWordsInParaParts <$> parprts
countWordsInBlock (BlockQuotes blcs) = countWordsInBlocks blcs
countWordsInBlock (BlockTech _ _ txt) = ceiling $ mathChars txt / 7
  where
    mathChars :: Text -> Double
    mathChars t = fromIntegral $ T.length t - numToDiscount t
    numToDiscount t = sum $ howManyChars <$> latexMathMacros <*> [t]
    howManyChars = (\x y -> T.length x * T.count x y) :: Text -> Text -> Int
    latexMathMacros = ["mbox", "left", "right", "equation","begin",
                       "end", "big", "mathrm", "textit", "textrm",
                       "textbf", "text", "mathtt", "mathfrak",
                       "mathcal", "cases"] :: [Text]


--NB: for tech blocks we count the number of characters and divided by 7 (the average
--length of an English word (circa 5) slightly ajusted to account for the verbosity
--of latex math strings). I also disregard well-known latex macro markers, which do
--not produce any visible character in the rendred document.**
--In constrast to block tech strings, each inline tech string is considered a word in
--and by itself (regardless of how many characters it has; see the `wordlike` function).
--
-- **There are many latex commands, e.g. "\gamma", "\begin{matrix}", "\times" etc which
--have a couple of characters but are rendred to just one character in the final ouptput.
--On our counting method, these will be counted as one word each, although they are just
--one fifth of an average word' length. Dividing by 7 (instead of 5) corrects this word
--inflation due to the latex math string verbosity, but it also makes the number of words
--in code blocks drop. Perhaps it's not worth obsessing over this now.


countWordsInBlocks :: [Block] -> Int
countWordsInBlocks blcs = sum $ countWordsInBlock <$> blcs

countWordsInSection :: Section -> Int
countWordsInSection (Section _ _ (Title inls) bdy secs) =
  countWordsInInlines inls +
  countWordsInBlocks (secblcs . secbdy $ bdy) +
  countWordsInSections secs
countWordsInSection (SecBlocks blcs) = countWordsInBlocks blcs
countWordsInSection (Meta mp) =
  countWordsInAuthor mp + countWordsInTitle mp + countWordsInTags mp + countWordsInAbstract mp

--meta fields

countWordsInAuthor :: M.Map MetaKey MetaValue -> Int
countWordsInAuthor mp = procAuthor mp (countWordsInInlines . concat) 0

countWordsInTitle :: M.Map MetaKey MetaValue -> Int
countWordsInTitle mp = procTitle mp countWordsInInlines 0

countWordsInTags :: M.Map MetaKey MetaValue -> Int
countWordsInTags mp = procTags mp countWordsInInlines 0

countWordsInAbstract :: M.Map MetaKey MetaValue -> Int
countWordsInAbstract mp = procAbstract mp countWordsInBlocks 0

countWordsInSections :: [Section] -> Int
countWordsInSections secs = sum $ countWordsInSection <$> secs

countWordsInDocument :: Document -> Int
countWordsInDocument (Doc secs) = countWordsInSections secs



------------
-- Select --
------------


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


----------------
-- Words with --
----------------

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


--------------------
-- Words equal to --
--------------------


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


--------------------------
-- Expressions equal to --
--------------------------


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


--------------
-- Quantify --
--------------

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


---------------
-- Positions --
---------------

--extract (line,column) position of inlines

 --type LineNo = Int
 --type ColumnNo = Int
 --type Position = (LineNo,ColumnNo)

inlinePos :: Inline -> Position
inlinePos (Word _ pos _) = pos
inlinePos (Citation _ _ pos _) = pos
inlinePos (InlineTech _ pos _) = pos
inlinePos (Number _ pos _) = pos
inlinePos (Email _ pos _ _) = pos
inlinePos _ = (0,0)

-- inlinesPos :: [Inline] -> (Position,Position)
-- inlinesPos inls =
--   let positions = inlinePos <$> inls
--   in (head positions, last positions)

emptypos :: Position
emptypos = (0,0)
emptyposes :: (Position,Position)
emptyposes = (emptypos,emptypos)


inlinesPos :: [Inline] -> (Position,Position)
inlinesPos [] = emptyposes
inlinesPos inls = (updatedStartingPos, updatedEndingPos)
  where inls' = removeInlineWrapers inls
        updatedStartingPos = subtractNonWordPos firstWordPos firstNonWords
        updatedEndingPos = accumulateNonWordPos lastWordPos lastNonWords

        --firstWordPos = inlinePos . head $ filter wordlike inls'
        firstWordPos = dealWithEmpty head inls'
        firstNonWords = takeWhile (not . wordlike) inls'
        --lastWordPos = inlinePos . last $ filter wordlike inls'
        lastWordPos = dealWithEmpty last inls'
        lastNonWords = takeWhile (not . wordlike) $ reverse inls'

        dealWithEmpty f ils | null $ filter wordlike ils = emptypos
                            | otherwise = inlinePos . f $ filter wordlike ils

        accumulateNonWordPos :: Position -> [Inline] -> Position
        accumulateNonWordPos pos [] = pos
        accumulateNonWordPos pos (nw:nws) =
          case nw of
            Space   -> accumulateNonWordPos (fst pos, snd pos + 1) nws
            Newline -> accumulateNonWordPos (fst pos + 1, 0) nws
            ParEnd  -> accumulateNonWordPos (fst pos + 2, 0) nws
            _       -> accumulateNonWordPos pos nws

        subtractNonWordPos :: Position -> [Inline] -> Position
        subtractNonWordPos pos [] = pos
        subtractNonWordPos pos (nw:nws) =
          case nw of
            Space   -> subtractNonWordPos (fst pos, snd pos - 1) nws
            Newline -> subtractNonWordPos (fst pos - 1, snd pos) nws
            ParEnd  -> subtractNonWordPos (fst pos - 2, snd pos) nws
            _       -> subtractNonWordPos pos nws


expressionPos :: Expression -> (Position,Position)
expressionPos (Expression inls) = inlinesPos inls

--get line numbers for AST elements

documentPos :: Document -> (Position, Position)
documentPos (Doc []) = emptyposes
documentPos (Doc secs) = (fst (divisionPos $ head secs), snd (divisionPos $ last secs))

divisionPos :: Section -> (Position, Position)
divisionPos (Section _ _ ttl _ subs) =
  let Title inls = ttl
      divEnd (Section _ _ _ _ sbs) = divEnd $ last sbs
      divEnd (SecBlocks blcs) = snd $ blockPos $ last blcs
      divEnd sec@(Meta _) = snd $ metaPos sec
  in (fst (inlinesPos (filter wordlike inls)), divEnd (last subs))
divisionPos (SecBlocks []) = emptyposes
divisionPos (SecBlocks blcs) = (fst (blockPos $ head blcs), snd (blockPos $ last blcs))
divisionPos sec@(Meta _) = metaPos sec

metaPos :: Meta -> (Position,Position)
metaPos sec = let inls = sortOn inlinePos $ getInlinesFromDiv sec
  in (fst (inlinesPos inls), snd (inlinesPos inls))

blockPos :: Block -> (Position, Position)
blockPos (Para prts) = paraPartsPos prts
blockPos (Footnote _ blcs)
  | null blcs = error "empty footnote; no position information"
  | otherwise = (fst (blockPos $ head blcs), snd (blockPos $ last blcs))
blockPos (BlockEx exs)
  | null exs = error "empty example block; no position information"
  | otherwise = let prts = unwrapExamples exs
                in (fst (paraPartPos $ head prts), snd (paraPartPos $ last prts))
blockPos (BlockQuotes blcs)
  | null blcs = error "empty block quote; no position information"
  | otherwise = (fst (blockPos $ head blcs), snd (blockPos $ last blcs))
blockPos (BlockTech _ pos txt) = (pos, textEndPos pos txt)
blockPos (LinkRef _ pos url) = (pos, textEndPos pos url)
blockPos (ImageRef _ pos pth) = (pos, textEndPos pos pth)
blockPos (Table prts) = paraPartsPos prts
blockPos (BlockComment blcs)
  | null blcs = error "empty comment block; no position information"
  | otherwise = (fst (blockPos $ head blcs), snd (blockPos $ last blcs))
blockPos _ = emptyposes

textEndPos :: Position -> Text -> Position
textEndPos pos txt =
  let parts = T.splitOn "\n" txt
  in (fst pos + length parts, T.length (last parts))

-- examplePos :: Example -> (Position, Position)
-- examplePos (MainEx _ _ _ prts) = paraPartsPos prts
-- examplePos (SubEx _ _ _ prts) = paraPartsPos prts

paraPartPos :: ParaPart -> (Position, Position)
paraPartPos (Sentence inls) = inlinesPos inls
paraPartPos (ParaFtn _ snts) = paraPartsPos snts
paraPartPos (Inlines inls) = inlinesPos inls
paraPartPos (Caption prts) = paraPartsPos prts
paraPartPos _ = ((0,0),(0,0))

paraPartsPos :: [ParaPart] -> (Position,Position)
paraPartsPos prts =
  let posTuples = fmap paraPartPos prts in (fst . head $ posTuples, snd . last $ posTuples)

--render position to text

textifyPosition :: Position -> Text
textifyPosition (lineNo,columnNo) =
  T.pack $ "line " ++ show lineNo ++ ", column " ++ show columnNo ++ ":"

textifyPositions :: (Position,Position) -> Text
textifyPositions ((lineNo1,columnNo1), (lineNo2,_)) =
  if lineNo1 /= lineNo2
    then T.pack $ "lines " ++ show lineNo1 ++ "-" ++ show lineNo2 ++
         ", column " ++ show columnNo1  ++ ":"
    else T.pack $ "lines " ++ show lineNo1 ++ ", column " ++ show columnNo1 ++ ":"

inlineTextPos :: Inline -> Text
inlineTextPos = textifyPosition . inlinePos

expressionTextPos :: Expression -> Text
expressionTextPos = textifyPosition . fst . expressionPos


--filter according to line number

wordsAtLines :: (Searchable a) => a -> M.Map Int [Inline]
wordsAtLines el = mapit $ (flip wordsAtLineNo $ el) <$> linenos
  where linenos = [1 .. linesOfText' el]

wordsAtLineNo :: (Searchable a) => Int -> a -> [Inline]
wordsAtLineNo n el = inlinesAtLineNo n $ filter wordlike (gatherInlines el)

inlinesAtLineNo' :: (Searchable a) => Int -> a -> [Inline]
inlinesAtLineNo' n el = inlinesAtLineNo n $ gatherInlines el

inlinesAtLineNo :: Int -> [Inline] -> [Inline]
inlinesAtLineNo _ [] = []
inlinesAtLineNo n (inl:inls)
  | inlineLineNo inl == n = inl : inlinesAtLineNo n inls
  | otherwise             = inlinesAtLineNo n inls

inlineLineNo :: Inline -> LineNo
inlineLineNo inl =
  case inl of
    Word _ (l,_) _       -> l
    Citation _ _ (l,_) _ -> l
    InlineTech _ (l,_) _ -> l
    Number _ (l,_) _     -> l
    Email _ (l,_) _ _    -> l
    _                    -> 0

inlineColumnNo :: Inline -> ColumnNo
inlineColumnNo inl =
  case inl of
    Word _ (_,c) _       -> c
    Citation _ _ (_,c) _ -> c
    InlineTech _ (_,c) _ -> c
    Number _ (_,c) _     -> c
    Email _ (_,c) _ _    -> c
    _                    -> 0

linesOfText :: Document -> Int
linesOfText doc = inlineLineNo $ last $ filter wordlike $ getInlines doc

linesOfText' :: (Searchable a) => a -> Int
linesOfText' el = inlineLineNo $ last $ filter wordlike $ gatherInlines el


--NB: relevant lines of text, namely lines of text where our target words begin





--filter according to count

greaterThan :: Int -> [[a]] -> [[a]]
greaterThan n = filter $ (>=n) . length

greaterThan' :: Int -> M.Map k [a] -> M.Map k [a]
greaterThan' n = M.filter $ (>=n) . length

--word and expression comparators

-- (*=*) :: Inline -> Inline -> Bool
-- (*=*) i@(Word _ _ txt1) j@(Word _ _ txt2) = txt1 == txt2
-- (*=*) i@(Citation _ _ _ txt1) j@(Citation _ _ _ txt2) = txt1 == txt2
-- (*=*) i@(InlineTech _ _ txt1) j@(InlineTech _ _ txt2) = txt1 == txt2
-- (*=*) i@(Number _ _  dbl1) j@(Number _ _ dbl2) = dbl1 == dbl2
-- (*=*) i j = i == j
--
-- (*/=*) :: Inline -> Inline -> Bool
-- (*/=*) i j = not $ i *=* j
--
-- ($=$) :: Expression -> Expression -> Bool
-- ($=$) e@(Expression inls1) f@(Expression inls2) = all true piecemealComparisons
--   where
--     true x = x == True
--     piecemealComparisons = [ inl1 *=* inl2 | (inl1, inl2) <- zip inls1 inls2 ]
--
-- ($/=$) :: Expression -> Expression -> Bool
-- ($/=$) e f = not $ e $=$ f
--
-- eq1 = Expression [Word [None] (0,0) "blah", Word [None] (0,0) "blah", Word [None] (0,0) "blah"] $=$
--   Expression [Word [None] (0,0) "blah", Word [None] (0,0) "hlab", Word [None] (0,0) "blah", Word [None] (0,0) "blah"]


----------
-- Tags --
----------


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

-- searching for word-shapes (e.g. suffix- or equality-based) and tags

taggedWordsInParaPart :: [Suffix] -> ParaPart -> [Inline]
taggedWordsInParaPart ends prt = wordsWithInInlines ends $
  adverbsInParaPart prt ++ adjectivesInParaPart prt

taggedWordsInBlock :: [Suffix] -> Block -> [Inline]
taggedWordsInBlock ends blc = wordsWithInInlines ends $
  adverbsInBlock blc ++ adjectivesInBlock blc




--tagged lookup

taggedLookup :: ParaIndex -> M.Map ParaIndex a -> Either Text a
taggedLookup i mp = note msg $ M.lookup i mp
  where msg = T.pack $ "I cannot find the block with index " ++ show i

taggedLookup' :: LineNo -> M.Map LineNo a -> Either Text a
taggedLookup' i mp = note msg $ M.lookup i mp
  where msg = T.pack $ "I cannot find line " ++ show i

taggedLookup'' :: No -> M.Map No a -> Either Text a
taggedLookup'' no mp = note msg $ M.lookup no mp
  where msg = T.pack $ "I cannot find section " ++ showSecNo no


wordlikeInSent :: Sentence -> [Inline]
wordlikeInSent (Sentence inls) = wordlikeInInlines inls
wordlikeInSent _ = []

wordlikeInInlines :: [Inline] -> [Inline]
wordlikeInInlines inls = filter wordlike (removeInlineWrapers inls)

-- wordlikeInInlines :: [Inline] -> [Inline]
-- wordlikeInInlines (inl:inls) =
--   case inl of
--     (Link inls' _)  -> filter wordlike inls' ++ wordlikeInInlines inls
--     (Image inls' _) -> filter wordlike inls' ++ wordlikeInInlines inls
--     _               -> if wordlike inl then (inl : wordlikeInInlines inls) else wordlikeInInlines inls


--------------
-- Sections --
--------------


--Get lists of items from Documents

getSections :: Document -> [Section]
getSections (Doc secs) = secs

getTitledSections :: Document -> [Section]
getTitledSections doc = filter isSection $ getSections doc

getAllSections :: Document -> [Section]
getAllSections doc = allSections $ getSections doc

getSection :: No -> Document -> Either T.Text Section
getSection no doc =
  let msg = T.pack $ "couldn't find section numbered " ++ showSecNo no
  in  note msg $ lookSectionUp no $ getAllSections doc

showSecNo :: No -> String
showSecNo no = intersperse '.' $ fmap intToDigit $ takeWhile (/= 0) (tupToList no)

lookSectionUp :: No -> [Section] -> Maybe Section
lookSectionUp _ [] = Nothing
lookSectionUp no (sec:secs)
  | sectionNo sec == no  = Just sec
  | otherwise            = lookSectionUp no secs
  where
    sectionNo sct@(Section _ _ _ _ _) = secno sct
    sectionNo _ = zeros


allSections :: [Section] -> [Section]
allSections [] = []
allSections (sect:sects) = disect sect ++ allSections sects
  where
    disect :: Section -> [Section]
    disect sec =
      case sec of
        s@(Section _ _ _ _ subs) -> s : concatMap disect subs
        _                        -> [sec]

sectionMap' :: [Section] -> M.Map No Section
sectionMap' [] = M.empty
sectionMap' (sec:secs) =
  case sec of
    Section no _ _ _ _ -> M.insert no sec $ sectionMap' secs
    _                  -> sectionMap' secs

sectionMap :: Document -> M.Map No Section
sectionMap doc = sectionMap' $ getAllSections doc




-- wordsAtLines :: Document -> M.Map Int [Inline]
-- wordsAtLines doc = mapit $ (flip wordsAtLineNo $ doc) <$> lines
--   where lines = [1 .. linesOfText doc]
--
-- wordsAtLineNo :: Int -> Document -> [Inline]
-- wordsAtLineNo n doc = inlinesAtLineNo n $ getWordLikes doc


-- sectionParts :: [Section] -> M.Map No (Title,[Section])
-- sectionParts [] = M.empty
-- sectionParts (sec@(Section no lev ttl divs):secs) =
--   M.insert (no,lev) (ttl,divs) $ sectionParts secs
--
-- sectionBodies :: [Section] -> M.Map No [Section]
-- sectionBodies sects = snd <$> sectionParts sects


------------
-- Blocks --
------------


getBlocks :: Document -> [Block]
getBlocks doc = divsToBlocks $ getSections doc

divsToBlocks :: [Section] -> [Block]
divsToBlocks [] = []
divsToBlocks (sec:secs) = unwrapSection' sec ++ divsToBlocks secs

unwrapSection' :: Section -> [Block]
unwrapSection' (SecBlocks blcs) = blcs
unwrapSection' (Section _ _ _ bdy secs) = (secblcs . secbdy $ bdy) ++ concatMap unwrapSection' secs
unwrapSection' (Meta mp) = let MetaBlocks blcs = getAbstract' mp in blcs
  where getAbstract' = M.findWithDefault (MetaBlocks []) AbstractKey

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


getQuotes :: Document -> [Quote]
getQuotes doc = filter isQuote $ getBlocks doc


---------------
-- ParaParts --
---------------


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


-------------
-- Inlines --
-------------


getInlines :: Document -> [Inline]
getInlines doc = concatMap unwrapParaPart' (getParaParts doc)

-- does not unwrap comments
unwrapParaPart' :: ParaPart -> [Inline]
unwrapParaPart' (Sentence inls) = inls
unwrapParaPart' (Inlines inls) = inls
unwrapParaPart' (ParaFtn _ snts) = concatMap unwrapParaPart' snts
unwrapParaPart' (Caption prts) = concatMap unwrapParaPart' prts
unwrapParaPart' _ = []

getWordLikes :: Document -> [Inline]
getWordLikes doc = filter wordlike $ getInlines doc

getCitations :: Document -> [Citation]
getCitations doc = filter isCit $ getInlines doc

getLinks :: Document -> [Link]
getLinks doc = filter isLink $ getInlines doc

getImages :: Document -> [Image]
getImages doc = filter isImage $ getInlines doc

getNumbers :: Document -> [Number]
getNumbers doc = filter isNumber $ getInlines doc

getPureWords :: Document -> [Word]
getPureWords doc = filter isWord $ getInlines doc


-------------------
-- From sections --
-------------------


--Get lists of items from Sections

getBlocksFromDiv :: Section -> [Block]
getBlocksFromDiv sec = unwrapSection' sec

getParasFromDiv :: Section -> [Paragraph]
getParasFromDiv sec = filter isParagraph $ unwrapSection' sec

getFootnotesFromDiv :: Section -> [Footnote]
getFootnotesFromDiv sec = getFootnoteBFromDiv sec ++ getFootnoteSFromDiv sec

getFootnoteSFromDiv :: Section -> [Footnote]
getFootnoteSFromDiv sec = parapartsToFootnotesS . getParaPartsFromDiv $ sec

getFootnoteBFromDiv :: Section -> [Footnote]
getFootnoteBFromDiv sec = blocksToFootnotesB . getBlocksFromDiv $ sec

getOrderedListsFromDiv :: Section -> [Example]
getOrderedListsFromDiv sec =
   filter isOrdered $ concatMap unwrapBlockEx $ filter isBlockEx (getBlocksFromDiv sec)

getUnorderedListsFromDiv :: Section -> [Example]
getUnorderedListsFromDiv sec =
   filter isUnordered $ concatMap unwrapBlockEx $ filter isBlockEx (getBlocksFromDiv sec)

getBlockQuotesFromDiv :: Section -> [Quote]
getBlockQuotesFromDiv sec = filter isQuote $ getBlocksFromDiv sec

getBlockTechFromDiv :: Section -> [BlockTech]
getBlockTechFromDiv sec = filter isBlockTech $ getBlocksFromDiv sec

getTablesFromDiv :: Section -> [Table]
getTablesFromDiv sec = filter isTable $ getBlocksFromDiv sec

getParaPartsFromDiv :: Section -> [ParaPart]
getParaPartsFromDiv sec = blocksToParaParts . getBlocksFromDiv $ sec

getSentencesFromDiv :: Section -> [Sentence]
getSentencesFromDiv sec = paraPartsToSents . getParaPartsFromDiv $ sec

getLongSentsFromDiv :: Int -> Section -> [Sentence]
getLongSentsFromDiv int sec = filter (overTheLimit int) (getSentencesFromDiv sec)

getShortSentsFromDiv :: Int -> Section -> [Sentence]
getShortSentsFromDiv int sec = filter (underTheLimit int) (getSentencesFromDiv sec)


getInlinePartsFromDiv :: Section -> [ParaPart]
getInlinePartsFromDiv sec = paraPartsToInlineParts . getParaPartsFromDiv $ sec

getInlinesFromDiv :: Section -> [Inline]
getInlinesFromDiv sec = concatMap unwrapParaPart' $ getParaPartsFromDiv sec

getCitationsFromDiv :: Section -> [Citation]
getCitationsFromDiv sec = filter isCit $ getInlinesFromDiv sec

getLinkFromDiv :: Section -> [Link]
getLinkFromDiv sec = filter isLink $ getInlinesFromDiv sec

getImageFromDiv :: Section -> [Image]
getImageFromDiv sec = filter isImage $ getInlinesFromDiv sec

getWordsFromDiv :: Section -> [Word]
getWordsFromDiv sec = filter isWord $ getInlinesFromDiv sec

getWordLikesFromDiv :: Section -> [Word]
getWordLikesFromDiv sec = filter wordlike $ getInlinesFromDiv sec


-----------------
-- From blocks --
-----------------


--get list items from Blocks

blocksToSentences :: [Block] -> [[Sentence]]
blocksToSentences blcs = fmap blockToSentences blcs

blockToSentences :: Block -> [Sentence]
blockToSentences blc = paraPartsToSents $ blockToParaParts blc

blockToInlines :: Block -> [Inline]
blockToInlines blc = concatMap unwrapParaPart' $ blockToParaParts blc

getWordLikesFromBlock :: Block -> [Inline]
getWordLikesFromBlock blc = filter wordlike $ blockToInlines blc

hasMoreThan :: Block -> Int -> Bool
hasMoreThan blc int = (length $ getWordLikesFromBlock blc) >= int

hasLessThan :: Block -> Int -> Bool
hasLessThan blc int = not $ hasMoreThan blc int

-- hasSentsWithMoreThan :: Block -> Int -> Bool
-- hasSentsWithMoreThan blc int =
--   let sentsInBlock = blockToSentences blc
--   in any overTheLimit sentsInBlock

overTheLimit :: Int -> Sentence -> Bool
overTheLimit int snt = (length $ getWordLikesFromSentence snt) >= int

underTheLimit :: Int -> Sentence -> Bool
underTheLimit int snt = (length $ getWordLikesFromSentence snt) < int

-- hasSentsWithLessThan :: Block -> Int -> Bool
-- hasSentsWithLessThan blc int = not $ hasSentsWithMoreThan blc int

getWordLikesFromSentence :: Sentence -> [Inline]
getWordLikesFromSentence = unwrapParaPart'

getLongSents :: Int -> Block -> [Sentence]
getLongSents int blc = filter (overTheLimit int) (blockToSentences blc)

getShortSents :: Int -> Block -> [Sentence]
getShortSents int blc = filter (underTheLimit int) (blockToSentences blc)


------------------------
-- Numbering Sections --
------------------------

--search sections

searchSection :: T.Text -> Document -> Either T.Text Section
searchSection txt doc =
  case txt of
    "meta"    -> getMeta doc
    "intro"   -> getIntro doc
    otherText -> getSection (mkNo otherText) doc

mkNo :: T.Text -> No
mkNo txt = listToTup $ intList txt ++ take r (repeat 0)
  where
    r = 6 - length (intList txt)
    intList t = read <$> T.unpack <$> T.splitOn ("." :: T.Text) t :: [Int]

mapSections :: [Section] -> M.Map T.Text Section
mapSections [] = M.empty
mapSections (sct:scts)
  | isMeta sct    = M.insert "meta" sct (mapSections scts)
  | isPureDiv sct = M.insert "intro" sct (mapSections scts)
  | otherwise     = M.insert (T.pack $ showSecNo $ secno sct) sct (mapSections scts)


-------------
-- Styling --
-------------

--handling styling and other attributes

isEmph :: Inline -> Bool
isEmph (Word attrs _ _) | Emph `elem` attrs = True
                        | otherwise         = False
isEmph _ = False

isBold :: Inline -> Bool
isBold (Word attrs _ _) | Bold `elem` attrs = True
                        | otherwise             = False
isBold _ = False


emph :: [Inline] -> [Inline]
emph inls = filter isEmph inls

bold :: [Inline] -> [Inline]
bold inls = filter isBold inls


emphDocPerBlock :: Document -> M.Map ParaIndex [Inline]
emphDocPerBlock doc = emph . getWordLikesFromBlock <$> (mapit $ getBlocks doc)

boldDocPerBlock :: Document -> M.Map ParaIndex [Inline]
boldDocPerBlock doc = bold . getWordLikesFromBlock <$> (mapit $ getBlocks doc)

emphInBlockNo :: ParaIndex -> Document -> Maybe [Inline]
emphInBlockNo i doc = M.lookup i $ emphDocPerBlock doc

boldInBlock :: ParaIndex -> Document -> Maybe [Inline]
boldInBlock i doc = M.lookup i $ boldDocPerBlock doc

emphDocPerSection :: Document -> M.Map No [Inline]
emphDocPerSection doc = emph . getInlinesFromDiv <$> sectionMap doc

boldDocPerSection :: Document -> M.Map No [Inline]
boldDocPerSection doc = bold . getInlinesFromDiv <$> sectionMap doc


emphInSectionNo :: Document -> No -> Maybe [Inline]
emphInSectionNo doc no = M.lookup no $ emphDocPerSection doc

boldInSection :: Document -> No -> Maybe [Inline]
boldInSection doc no = M.lookup no $ boldDocPerSection doc



emphInDoc :: Document -> [Inline]
emphInDoc doc = filter isEmph $ getInlines doc

boldInDoc :: Document -> [Inline]
boldInDoc doc = filter isBold $ getInlines doc

emphCountInDoc :: Document -> Int
emphCountInDoc doc = length $ emphInDoc doc

boldCountInDoc :: Document -> Int
boldCountInDoc doc = length $ boldInDoc doc

-----------------------
-- Example Numbering --
-----------------------

numExamples :: Int -> [Example] -> [Example]
numExamples _ [] = []
numExamples n (ex:exs) =
  let intToNo i = (i,0,0,0,0,0)
  in  case ex of
        Example Ordered lev _ nom bdy exs' ->
          (Example Ordered lev (intToNo n) nom bdy exs') : numExamples (n + 1) exs
        _ -> numExamples (n + 1) exs

numberExamples :: Document -> [Example]
numberExamples doc = numExamples 1 $ gatherExamples doc


----------------
-- Attributes --
----------------


-- attributes

hasAttrs' :: (Searchable a) => a -> [Attr] -> Bool
hasAttrs' el attrs =
  let inls = gatherInlines el -- filter wordlike . gatherInlines $ el
  in  all (`hasAttrsInline` attrs) inls

hasExactlyAttrs'  :: (Searchable a) => a -> [Attr] -> Bool
hasExactlyAttrs' el attrs =
  let inls = gatherInlines el
  in  all (`hasExactlyAttrsInline` attrs) inls

hasSomeAttrs' :: (Searchable a) => a -> [Attr] -> Bool
hasSomeAttrs' el attrs =
  let inls = gatherInlines el
  in  all (`hasAttrsInline` attrs) inls

someHaveAttrs' :: (Searchable a) => a -> [Attr] -> Bool
someHaveAttrs' el attrs =
  let inls = gatherInlines el
  in  someHaveAttrsInlines inls attrs

hasAttrInline :: Inline -> Attr -> Bool
hasAttrInline (Word attrs _ _) attr       = attr `elem` attrs
hasAttrInline (Citation attrs _ _ _) attr = attr `elem` attrs
hasAttrInline (Number attrs _ _) attr     = attr `elem` attrs
hasAttrInline (Email attrs _ _ _) attr    = attr `elem` attrs
hasAttrInline _ _                         = False

hasExactlyAttrsInline :: Inline -> [Attr] -> Bool
hasExactlyAttrsInline (Word attrs _ _) attrs'       = attrs == attrs'
hasExactlyAttrsInline (Citation attrs _ _ _) attrs' = attrs == attrs'
hasExactlyAttrsInline (Number attrs _ _) attrs'     = attrs == attrs'
hasExactlyAttrsInline (Email attrs _ _ _) attrs'    = attrs == attrs'
hasExactlyAttrsInline _ _                           = False



hasAttrsInline :: Inline -> [Attr] -> Bool
hasAttrsInline inl attrs = all (inl `hasAttrInline`) attrs

hasSomeAttrsInline :: Inline -> [Attr] -> Bool
hasSomeAttrsInline inl attrs = any (inl `hasAttrInline`) attrs

someHaveAttrsInlines :: [Inline] -> [Attr] -> Bool
someHaveAttrsInlines inls attrs = all (`hasSomeAttrsInline` attrs) inls

haveAttrInlines :: [Inline] -> Attr -> Bool
haveAttrInlines inls attr = all (`hasAttrInline` attr) (filter wordlike inls)




getAttrs :: Inline -> [Attr]
getAttrs inl =
  case inl of
   Word attrs _ _        -> attrs
   Citation attrs _ _ _  -> attrs
   Number attrs _ _      -> attrs
   Email attrs _ _ _     -> attrs
   _                     -> []


addEmph :: Inline -> Inline
addEmph = addAttr Emph

addBold :: Inline -> Inline
addBold = addAttr Bold

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


----------
-- Meta --
----------

--dealing with meta

procAuthor :: M.Map MetaKey MetaValue -> ([[Inline]] -> res) -> res -> res
procAuthor mp succsr fails =
  case (M.lookup AuthorKey mp) of
    Just (MetaSeq inlss) -> succsr inlss
    _                    -> fails

procTitle :: M.Map MetaKey MetaValue -> ([Inline] -> res) -> res -> res
procTitle mp succsr fails =
  case (M.lookup TitleKey mp) of
    Just (MetaInlines inls) -> succsr inls
    _                       -> fails

procAbstract :: M.Map MetaKey MetaValue -> ([Block] -> res) -> res -> res
procAbstract mp succsr fails =
  case (M.lookup AbstractKey mp) of
    Just (MetaBlocks blcs) -> succsr blcs
    _                       -> fails

procTags :: M.Map MetaKey MetaValue -> ([Inline] -> res) -> res -> res
procTags mp succsr fails =
  case (M.lookup TagKey mp) of
    Just (MetaInlines inls) -> succsr inls
    _                       -> fails

--the number of authors is given by

noAuthors :: M.Map MetaKey MetaValue -> Int
noAuthors mp = procAuthor mp length 0 :: Int

-- estimated reading time using an average of 200 words per minute
readingTime :: Searchable a => a -> Int
readingTime el = let wpm = 200 in countWordsIn el `div` wpm


-- to be used by the command-line tool to display the Meta section
metaToText :: Section -> [Text]
metaToText mt =
  let Expressions exprs = repackageMeta mt
      exprsToText [] = []
      exprsToText (e:es) =
        let Expression inls = e in inlinesToTxt inls : exprsToText es
  in  exprsToText exprs


repackageMeta :: Section -> ParaPart
repackageMeta (Meta mp) =
  let ttl = let MetaInlines inls = M.findWithDefault (MetaInlines []) TitleKey mp
            in  Expression $ filter wordlike inls
      ath = M.findWithDefault (MetaInlines []) AuthorKey mp
      abstr = let MetaBlocks blcs = M.findWithDefault (MetaBlocks []) AbstractKey mp
             in  [Expression $ concatMap (filter wordlike . gatherInlines) blcs]
      ttlAndAth =
        case ath of
          MetaInlines inls -> ttl : [Expression $ filter wordlike inls]
          MetaSeq inlss    -> ttl : fmap (Expression . filter wordlike) inlss
          _                -> []
  in  Expressions $ filter (/= Expression []) (ttlAndAth ++ abstr)
repackageMeta _ = Inlines []


-- toc

toc :: Document -> Text
toc doc = T.intercalate "\n" $ sectionEntry <$> getAllSections doc


sectionEntry :: Section -> Text
sectionEntry sct =
  let no = (T.pack $ showSecNo $ secno sct) `T.append` " "
      ttl = let Title inls = secttl sct in T.toTitle (inlinesToTxt inls) `T.append` " "
      pos = let p = fst (getPosition sct) in "position " `T.append` posToTxt p
      cnt = "words " `T.append` (T.pack $ show $ countWordsIn sct)
  in  foldr1 T.append  [no, ttl, "(", pos, ", ", cnt, ")"]


-- textify

posToTxt :: Position -> Text
posToTxt (l,c) = T.pack (show l) `T.append` ":" `T.append` T.pack (show c)

inlinesToTxt :: [Inline] -> Text
inlinesToTxt inls = T.concat $ fmap inlineToTxt inls

inlineToTxt :: Inline -> Text
inlineToTxt (Word _ _ txt) = txt
inlineToTxt (Citation _ _ _ txt) = txt
inlineToTxt (InlineTech _ _ txt) = txt
inlineToTxt (InlineComment inls) = T.concat $ inlineToTxt <$> inls
inlineToTxt (Punct _ txt) = txt
inlineToTxt (Number _ _ n) = T.pack $ show n
inlineToTxt (Link _ urlid) = urlid
inlineToTxt (Image _ pthid) = pthid
inlineToTxt (Email _ _ nom dom) = nom `T.append` "@" `T.append` dom
inlineToTxt Space = " "
inlineToTxt Newline = "\n"
inlineToTxt ParEnd = "\n\n"
inlineToTxt _ = ""

-- get meta info

getAuthors :: forall a. Searchable a => a -> Either T.Text Expressions
getAuthors el =
  let expin m = let Meta emeta = m in mkexp <$> note "no authors found" (M.lookup AuthorKey emeta)
      mkexp (MetaSeq inlss) = Expressions $ fmap Expression inlss
      mkexp (MetaInlines inls) = Inlines $ filter wordlike inls
      mkexp _ = Inlines []
  in  expin =<< getMeta' el

getTitle :: forall a. Searchable a => a -> Either T.Text Inlines
getTitle el =
  let titleof m = let Meta emeta = m in mkttl <$> note "no title found" (M.lookup TitleKey emeta)
      mkttl (MetaInlines inls) = Inlines $ filter wordlike inls
      mkttl _ = Inlines []
  in  titleof =<< getMeta' el

getAbstract :: forall a. Searchable a => a -> Either T.Text Inlines
getAbstract el =
  let abstract m = let Meta emeta = m in mkabs <$> note "no abstract found" (M.lookup AbstractKey emeta)
      mkabs (MetaBlocks blcs) = Inlines $ getWordLikesFromDiv (SecBlocks blcs)
      mkabs _ = Inlines []
  in  abstract =<< getMeta' el


getMeta' :: forall a. Searchable a => a -> Either T.Text Meta
getMeta' el =
  let msg = T.pack $ "couldn't find any meta section "
  in  note msg $ case (filter isMeta $ gatherSections el) of
                   []     -> Nothing
                   metas  -> Just (head metas)

getMeta :: Document -> Either T.Text Meta
getMeta doc =
  let msg = T.pack $ "couldn't find any meta section "
  in  note msg $ case (filter isMeta $ getSections doc) of
                   []     -> Nothing
                   metas  -> Just (head metas)

getIntro :: Document -> Either T.Text Section
getIntro doc =
  let msg = T.pack $ "couldn't find any intro division before the beginning of sections "
  in  note msg $ case (filter isPureDiv $ getSections doc) of
                   []     -> Nothing
                   secs -> Just (head secs)




