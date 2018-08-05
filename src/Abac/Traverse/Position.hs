{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.Position where

import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Text as T (Text, splitOn, length, isSuffixOf,
                                 empty, append, intercalate,
                                 replicate, unwords, pack, unpack,
                                 count, concat, cons, snoc,)
import Text.Megaparsec (SourcePos)
import Text.Megaparsec.Pos (unPos,sourceLine,sourceColumn)
import Prelude hiding (Word)





import Abac.Types.ParserTypes
import Abac.Internal

import Abac.Traverse.Internal
import Abac.Traverse.Predicates
import Abac.Traverse.Sections (getInlinesFromDiv)
--import Abac.Traverse.Searchable


--extract (line,column) position of inlines

 --type LineNo = Int
 --type ColumnNo = Int
 --type Position = (LineNo,ColumnNo)

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

inlinesPos :: [Inline] -> (Position,Position)
inlinesPos inls = (updatedStartingPos, updatedEndingPos)
  where inls' = removeInlineWrapers inls
        updatedStartingPos = subtractNonWordPos firstWordPos firstNonWords
        updatedEndingPos = accumulateNonWordPos lastWordPos lastNonWords

        firstWordPos = inlinePos . head $ filter wordlike inls'
        firstNonWords = takeWhile (not . wordlike) inls'
        lastWordPos = inlinePos . last $ filter wordlike inls'
        lastNonWords = takeWhile (not . wordlike) $ reverse inls'

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
documentPos (Doc secs) = (fst (divisionPos $ head secs), snd (divisionPos $ last secs))

divisionPos :: Section -> (Position, Position)
divisionPos (Section _ _ ttl _ subs) =
  let Title inls = ttl
      divEnd (Section _ _ _ _ sbs) = divEnd $ last sbs
      divEnd (SecBlocks blcs) = snd $ blockPos $ last blcs
      divEnd sec@(Meta _) = snd $ metaPos sec
  in (inlinePos (head $ filter wordlike inls), divEnd (last subs))
divisionPos (SecBlocks []) = ((0,0),(0,0))
divisionPos (SecBlocks blcs) = (fst (blockPos $ head blcs), snd (blockPos $ last blcs))
divisionPos sec@(Meta _) = metaPos sec

metaPos :: Meta -> (Position,Position)
metaPos sec = let inls = sortOn inlinePos $ getInlinesFromDiv sec
  in (inlinePos (head inls), inlinePos (last inls))

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
blockPos _ = ((0,0),(0,0))

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

