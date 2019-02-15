{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.InternalInternal where

import qualified Data.Map.Strict as M
import qualified Data.Text as T (Text, splitOn, length, isSuffixOf,
                                 empty, append, intercalate,
                                 replicate, unwords, pack, unpack,
                                 count, concat, cons, snoc)

import Data.List
import Data.Char (intToDigit)
import Control.Error.Util (note)
import Prelude hiding (Word)


import Abac.Types.ParserTypes
import Abac.Traverse.Predicates
--import Abac.Traverse.Count
--import Abac.Traverse.Words
--import Abac.Traverse.Expressions

-- auxiliary functions used to number sections and examples

-- e.g. turns (1,1,0,0,1,0) to (1,1,1,1,1,0)
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

--converting No to [Int] and back; used for numbering in examples and sections
tupToList :: No -> [Int]
tupToList (i,j,k,l,m,n) = [i,j,k,l,m,n]

listToTup :: [Int] -> No
listToTup [i,j,k,l,m,n] = (i,j,k,l,m,n)
listToTup _ = zeros


type ParaIndex = Int

--lists of elements will be made into maps

mapit :: [a] -> M.Map Int a
mapit xs = M.fromList $ zip [1 ..] xs



-- We remove the Link and Image wrapers (which wrap a list of inlines) and
-- add the wrapped inlines to the initial list of inlines (the argument).

removeInlineWrapers :: [Inline] -> [Inline]
removeInlineWrapers [] = []
removeInlineWrapers (inl:inls) =
  case inl of
    (Link inls' _)  -> inls' ++ removeInlineWrapers inls
    (Image inls' _) -> inls' ++ removeInlineWrapers inls
    _               -> inl : removeInlineWrapers inls

-- make words

mkWords :: [Text] -> [Word]
mkWords txts = mkWord <$> txts

mkWord :: Text -> Word
mkWord = Word [None] (0,0)


--filter according to count

greaterThan :: Int -> [[a]] -> [[a]]
greaterThan n = filter $ (>=n) . length

greaterThan' :: Int -> M.Map k [a] -> M.Map k [a]
greaterThan' n = M.filter $ (>=n) . length


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

-- render section number (a 6-tuple) as string
showSecNo :: No -> String
showSecNo no = intersperse '.' $ fmap intToDigit $ takeWhile (/= 0) (tupToList no)

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

--unwrap type one level down

unwrapExamples :: [Example] -> [ParaPart]
unwrapExamples [] = []
unwrapExamples exs = concatMap unwrapExample exs

unwrapExample :: Example -> [ParaPart]
unwrapExample (Example _ _ _ _ bdy []) = exbdy bdy
unwrapExample (Example _ _ _ _ _ exs) = concatMap unwrapExample exs


-- get inlines

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

--Give me the number of words hosted by the Eithers type Right, if any

number :: (Num c, Functor f) => f [b] -> f c
number eitherVal = (fromIntegral . length) <$> eitherVal

--a version of isSuffixOf that works over Words

isMySuffixOf :: Suffix -> Word -> Bool
isMySuffixOf (Word _ _ end) (Word _ _ wrd) = end `T.isSuffixOf` wrd
isMySuffixOf _ _ = False

