module Abac.Traverse.Searchable where

import Data.List (delete)
import Prelude hiding (Word)



import Abac.Types.ParserTypes
import Abac.Internal

import Abac.Traverse.Predicates
import Abac.Traverse.Attributes
import Abac.Traverse.Internal
import Abac.Traverse.Position
--import Abac.Traverse.Expressions

--A class for AST elements, Inline, Sentence, Block, Section, Doc
class Element a

instance Element Inline
instance Element Expression
instance Element ParaPart
instance Element Block
instance Element Section
instance Element Document

class Searchable a where

  -- search based on word equality and constitution
  wordsWith              :: [Suffix] -> a -> [Inline]
  wordsEqualTo           :: [Word] -> a -> [Word]
  expressionsWith        :: Int -> [Word] -> a -> [Expression]
  expressionsEqualTo     :: [Expression] -> a -> [Expression]
  countWordsWith         :: [Suffix] -> a -> Int
  proportionWordsWith    :: [Suffix] -> a -> Double
  countWordsEqualTo      :: [Word] -> a -> Int
  proportionWordsEqualTo :: [Word] -> a -> Double
  countExpressions       :: [Expression] -> a -> Int
  proportionExpressions  :: [Expression] -> a -> Double

  -- extract tree elements from larger tree elements
  ngramsIn               :: Int -> a -> [Expression]
  gatherInlines          :: a -> [Inline]
  gatherParaParts        :: a -> [ParaPart]
  gatherBlocks           :: a -> [Block]
  gatherSections         :: a -> [Section]
  getPosition            :: a -> (Position,Position)
  gatherExamples :: a -> [Example]
  gatherExamples el =
    let exblcs = filter isBlockEx $ gatherBlocks el
        unwrapExBlcs (BlockEx exs) = exs
        unwrapExBlcs _ = []
    in  concatMap unwrapExBlcs exblcs



  -- word list and count
  wordsCommon            :: a -> [Inline]
  wordsCommon el = filter wordlike $ gatherInlines el
  countWordsIn           :: a -> Int
  countWordsIn el = length $ wordsCommon el


  -- attributes and attribute-based searches and updates
  addAttr                :: Attr -> a -> a

  hasAttrs               :: a -> [Attr] -> Bool
  hasAttrs = hasAttrs'

  hasExactlyAttrs        :: a -> [Attr] -> Bool
  hasExactlyAttrs = hasExactlyAttrs'

  someHaveAttrs          :: a -> [Attr] -> Bool
  someHaveAttrs = someHaveAttrs'

  hasSomeAttrs           :: a -> [Attr] -> Bool
  hasSomeAttrs = hasSomeAttrs'

  traverseWith           :: (Inline -> Bool) -> a -> [Inline]
  traverseWith predic el = filter predic $ gatherInlines el

  traverseWith'           :: Int -> (Expression -> Bool) -> a -> [Expression]
  traverseWith' n predic el = filter predic $ ngramsIn n el


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

-- filter according to attributes

inlinesWithAttrsInInlines :: [Inline] -> [Attr] -> [Inline]
inlinesWithAttrsInInlines inls attrs = filter (flip hasAttrsInline $ attrs) inls

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


