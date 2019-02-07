{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.Examples where

import Prelude hiding (Word,pred)

import Control.Applicative hiding ((<|>),many,some)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (newline)

import Abac.Types.ParserTypes
import Abac.Internal (isOrdered,listToTup,tupToList)

import Abac.Parser.Operations
import Abac.Parser.ParaParts
import Abac.Parser.Inlines
import Abac.Parser.Markers

--Constraints for ParaPart values appearing in a single example item:

-- - inlines can follow sentences but sentences cannot follow inlines; this is natural
-- given that sentences are established after identifying a end-of-sentence punctuation
-- character; so, if we start to parse for a group of inlines followed by a sentence (e.g.
-- " first item, second item, third item\n This is a sentence.), the inlines will be
-- parsed as part of the sentence, rather than as an idependent group.
-- - of course, within each example item there can be sentence-only and inlines-only values
-- (constructed with the Sentence and Inlines constructors, respectively)
-- - footnotes can occur either after the end of a sentence or at the end of a group of
-- inlines (viz. value constructed with Inlines)
-- - subitems of numbered examples may be unnumbered, but not vice versa; see subExample
-- vs subItem

updateExamplesWith :: No -> [Example] -> [Example]
updateExamplesWith no exs = updateSubexamples $ updateMainExamplesWith no exs

updateMainExamplesWith :: No -> [Example] -> [Example]
updateMainExamplesWith no exs = zipWith numberSubexamples (genNextSecNoRange no) exs

updateSubexamples :: [Example] -> [Example]
updateSubexamples [] = []
updateSubexamples (ex:exs) =
  case ex of
    Example _ _ _ _ _ []        -> ex : updateSubexamples exs
    ex'@(Example _ _ _ _ _ _) ->
      let Example ord lev no' nom bd exs' = numberSubexamples no' ex'
      in  Example ord lev no' nom bd (updateSubexamples exs') : updateSubexamples exs


numberSubexamples :: No -> Example -> Example
numberSubexamples no ex = if isOrdered ex then no `updateExampleWith` ex else ex

updateExampleWith :: No -> Example -> Example
updateExampleWith no (Example ord lev _ nom bdy exs) =
  Example ord lev no nom bdy (zipWith numberSubexamples (genNextSecNoRange no) exs)

updateSubexampleNos :: No -> Example -> Example
updateSubexampleNos no (Example ord lev _ nom bd exs) = Example ord lev no nom bd exs


--generate ranges of example numbers

genNextSecNo :: No -> No
genNextSecNo no =
  let ints = tupToList no
  in  listToTup $ addOne 1 ints

genNextSecNoRange :: No -> [No]
genNextSecNoRange no =
  let ints = tupToList no
  in  (listToTup . \n -> addOne n ints) <$> [1 .. ]

addOne :: Int -> [Int] -> [Int]
addOne n ints
  | all (== 0) ints = (n : drop 1 ints)
  | otherwise =
      let nonzeros = takeWhile (/= 0) ints
          zeros' = dropWhile (/= 0) ints
      in  init nonzeros ++ [last nonzeros + n] ++ zeros'


genSubsecNo :: No -> No
genSubsecNo no =
  let ints = tupToList no
  in  listToTup $ flip0To 1 ints

genSubsecNoRange :: No -> [No]
genSubsecNoRange no =
  let ints = tupToList no
  in (listToTup  . \n -> flip0To n ints) <$> [1 .. ]

flip0To :: Int -> [Int] -> [Int]
flip0To _ [] = []
flip0To n (int : ints)
  | int == 0  = n : ints
  | otherwise = int : flip0To n ints


--BlockEx [Example]

blockex :: Parser Block
blockex = BlockEx <$> examplesNoNewline
{- Note: examplesNoNewline is a parser that does not require
   a newline before the example-marker. It is meant to replace
   the examples parser -}

--Example OrderParam Level No Name Body [Example]
examplesNoNewline :: Parser [Example]
examplesNoNewline = (subordinatesNoNewline 0) -- <* lookAhead parend

examples :: Parser [Example]
examples = subordinates 0 -- <* lookAhead parend

--a parser that parses a sequence of examples of level l, where the last example can
--optionally have a bunch of sub-examples.

subordinatesNoNewline :: Level -> Parser [Example]
subordinatesNoNewline _ = do
  expsNoNewline <- (:[]) <$> (try mrkPlusBodyNoNewline <|> mrkPlusBody)
  exps <- (some $ try mrkPlusBody) <|> return []
  lookAhead parend
  return $ embedall (expsNoNewline ++ exps)

subordinates :: Level -> Parser [Example]
subordinates _ = do
  exps <- some $ try mrkPlusBody
  lookAhead parend
  return $ embedall exps

--handing embedding of subexamples in their superexamples

embedall :: [Example] -> [Example]
embedall [] = []
embedall [ex] = [ex]
embedall (ex1 : ex2 : exs)
  | ex2 `isLevel` (== lev1) = ex1 : embedall (ex2 : exs)
  | ex2 `isLevel` (>= lev1) = embedall (ex1 `embeds` ex2 : exs)
  | otherwise               = ex1 : embedall (ex2 : exs) -- this will produce stranded, higher level
  where                                                  -- example items; see Ex 2 below:
    lev1 = exampleLevel ex1                              -- [Ex 2 _ _ _ _, Ex 0 _ _ _ [Ex 1 _ _ _ _]]

embeds :: Example -> Example -> Example
embeds ex1 ex2 = go dif ex1 ex2
  where
    dif = exampleLevel ex2 - exampleLevel ex1
    go :: Int -> Example -> Example -> Example
    go 1 (Example ord lev no nom bdy exs) ex' = Example ord lev no nom bdy (exs ++ [ex'])
    go i (Example ord lev no nom bdy exs) ex'
       | null exs = Example ord lev no nom bdy (ex' : exs)
       | otherwise = Example ord lev no nom bdy (init exs ++ [go (i - 1) (last exs) ex'] )

isLevel :: Example -> (Int -> Bool) -> Bool
isLevel ex pred | pred $ exampleLevel ex = True
                | otherwise              = False


embedin :: [Example] -> [Example] -> [Example]
embedin subs [] = subs
embedin [] exps = exps
embedin subs exps = init exps ++ [subs `embed` last exps]
  where
    embed :: [Example] -> Example -> Example
    embed exs (Example ord lev no nom body sbs) = Example ord lev no nom body (sbs ++ exs)

defex :: Example
defex = Example Unordered 0 zeros "noname" (ExBody []) [] :: Example



--example parts

mrkPlusBodyNoNewline :: Parser Example
mrkPlusBodyNoNewline = ( do
  mark <- markerNoNewline
  prts <- (many $ try paraPart) -- exampleParaParts
  return $ updateExampleUsing (ExBody prts) mark )


mrkPlusBody :: Parser Example
mrkPlusBody = ( do
  mark <- newline *> markerNoNewline
  prts <- (many $ try paraPart) -- exampleParaParts
  return $ updateExampleUsing (ExBody prts) mark )

exampleParaParts :: Parser [ParaPart]
exampleParaParts = someParaPart `manyTill` lookAhead (try inlineMarker <|> parend)

updateExampleUsing :: ExBody -> Marker -> Example
updateExampleUsing bdy amrk = updateExampleBody bdy $ defex `inheritParamsFrom` amrk

inheritParamsFrom :: Example -> Marker -> Example
inheritParamsFrom (Example _ _ _ _ body subs) (ExMark lev no nom) =
  Example Ordered lev no nom body subs
inheritParamsFrom (Example _ _ _ _ body subs) (ItMark lev no nom) =
  Example Unordered lev no nom body subs
inheritParamsFrom ex _ = ex 

mrkLevelUp :: Marker -> Parser Marker
mrkLevelUp amrk = let lev = markerLevel amrk in checkMarkerWith (< lev)

mrkLevelUp' :: Level -> Parser Marker
mrkLevelUp' lev = checkMarkerWith (< lev)

checkMarkerWith :: (Level -> Bool) -> Parser Marker
checkMarkerWith pred = do
  anymrk <- try exmrk <|> itmrk
  check anymrk
  where
    check :: Marker -> Parser Marker
    check mrk' =
      if pred $ markerLevel mrk'
         then return mrk'
         else fail $ "level " ++ show (markerLevel mrk') ++ " marker was not expected"


--examples' levels and numbers

exampleLevel :: Example -> Level
exampleLevel (Example _ lev _ _ _ _) = lev

exampleNo :: Example -> No
exampleNo (Example _ _ no _ _ _) = no

exampleSubparts :: Example -> [Example]
exampleSubparts (Example _ _ _ _ _ prts) = prts

updateExampleLevel :: Level -> Example -> Example
updateExampleLevel lev (Example ord _ no nom body subs) = Example ord lev no nom body subs

updateExampleNo :: No -> Example -> Example
updateExampleNo no (Example ord lev _ nom body subs) = Example ord lev no nom body subs

updateExampleBody :: ExBody -> Example -> Example
updateExampleBody body (Example ord lev no nom _ subs) = Example ord lev no nom body subs


