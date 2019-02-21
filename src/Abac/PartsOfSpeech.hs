{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Abac.PartsOfSpeech

  ( module Abac.PartsOfSpeech.Counters
  , module Abac.PartsOfSpeech.Sentences
  , module Abac.PartsOfSpeech
  ) where

import Abac.PartsOfSpeech.Counters
import Abac.PartsOfSpeech.Sentences

import Abac.Types
import Abac.Traverse
import Abac.Internal


import qualified Data.Text as T
import qualified Data.Map as M
import Control.Error.Util (note)


import Prelude hiding (Word)

-- meta filters

author :: forall a. Searchable a => a -> Abac Res
author dcm =
  let aut = getAuthors dcm
      autno (Expressions exs) = length exs
      autno (Inlines inls) = length inls
      autno _ = (-1)
  in  case aut of
        Left err -> Abc $ returnIO $ Left $ Signal err
        Right x -> Abc $ returnIO $ Right $
          Res { resElems = ListResult (putInExpr x)
              , resCount = CountResult (autno x)
              , resPercent = PercentResult 1
              }

title :: forall a. Searchable a => a -> Abac Res
title dcm =
  let ttl = getTitle dcm
      no (Inlines inls) = length inls
      no _ = (-1)
  in  case ttl of
        Left err -> Abc $ returnIO $ Left $ Signal err
        Right il -> Abc $ returnIO $ Right $
          Res { resElems = ListResult (putInExpr il)
              , resCount = CountResult (no il)
              , resPercent = PercentResult 1
              }

abstract :: forall a. Searchable a => a -> Abac Res
abstract dcm =
  let abstr = getAbstract dcm
      no (Inlines inls) = length inls
      no _ = (-1)
  in  case abstr of
        Left err -> Abc $ returnIO $ Left $ Signal err
        Right ab -> Abc $ returnIO $ Right $
          Res { resElems = ListResult (putInExpr ab)
              , resCount = CountResult (no ab)
              , resPercent = PercentResult 1
              }

returnIO :: a -> IO a
returnIO x = return x

-- Expressions display differently (with position of leading word)
-- and sometimes it'd be useful to convert Inlines to Expressions
putInExpr :: ParaPart -> ParaPart
putInExpr (Inlines inls) = Expressions [Expression inls]
putInExpr ex = ex


-- style filters etc.

email :: forall a. (Searchable a) => a -> Abac (Res)
email = fromFilter isEmail

citation :: (Searchable a) => a -> Abac (Res)
citation = fromFilter isCit

number' :: (Searchable a) => a -> Abac (Res)
number' = fromFilter isNumber

math :: (Searchable a) => a -> Abac (Res)
math = fromFilter isMath

code :: (Searchable a) => a -> Abac (Res)
code = fromFilter isCode

emph' :: (Searchable a) => a -> Abac (Res)
emph' = fromFilter isEmph

bold' :: (Searchable a) => a -> Abac (Res)
bold' = fromFilter isBold

tagged :: (Searchable a) => Tag -> a -> Abac (Res)
tagged t = fromFilter (\inl -> isTaggedWith inl t)

--wrapped version
tagged' :: (Searchable a) => TagParam -> a -> Abac (Res)
tagged' t' = let TagParam t = t' in fromFilter (\inl -> isTaggedWith inl (Tag t))

parened :: (Searchable a) => a -> Abac (Res)
parened = fromFilter (`hasAttrs` [Parenthetical])

bracketed :: (Searchable a) => a -> Abac (Res)
bracketed = fromFilter (`hasAttrs` [Bracketed])

sngquoted :: (Searchable a) => a -> Abac (Res)
sngquoted = fromFilter (`hasAttrs` [Quoted])

dblquoted :: (Searchable a) => a -> Abac (Res)
dblquoted = fromFilter (`hasAttrs` [DoubleQuoted])


-- accessor functions

list :: Res -> Abac ListResult
list res = resElems <$> return res

count :: Res -> Abac CountResult -- Int
count res = resCount <$> return res

percent :: Res -> Abac PercentResult -- Double
percent res = resPercent <$> return res

time :: Res -> Abac TimeResult
time res = do
  CountResult n <- resCount <$> return res
  let wpm = 200
  let minutes = n `div` wpm
  return $ TimeResult minutes

-- positional filters

line :: (Searchable a) => Int -> a -> Abac Inlines
line n el =
  let originals = M.filter (/= Inlines []) $ Inlines <$> wordsAtLines el
      relatives = mapit $ M.elems originals
      minls = M.lookup n relatives
      msg = Signal $ T.pack $ "Can't find the line number " ++ show n
  in  Abc $ return $ note msg minls

section :: forall a. (Searchable a) => Text -> a -> Abac Section
section txtkey el =
  let secmp = mapSections . allSections . gatherSections $ el
      msg = Signal $ T.append (T.pack "Can't find the section ") txtkey
  in  Abc $ return $ note msg $ M.lookup txtkey $ secmp


paragraph :: forall a. (Searchable a) => Int -> a -> Abac Paragraph
paragraph n el =
  let mpar = M.lookup n $ mapit $ gatherBlocks el
      msg = Signal $ T.pack $ "there is no paragraph numbered " ++ show n
  in  Abc $ return $ note msg mpar



-- grammatical filters

ngram :: (Searchable a) => Int -> a -> Abac (Res) -- Expressions
ngram n el =
  let ngms = ngramsIn n el
  in  return $ Res (ListResult $ Expressions ngms) (CountResult $ length ngms) (PercentResult 1)

anyword :: (Searchable a) => a -> Abac (Res)
anyword el =
  let wrds = filter wordlike (gatherInlines el)
      inls = ListResult $ Inlines wrds
      num = CountResult $ length wrds
      prop = PercentResult 1
  in return $ Res inls num prop

adword :: (Searchable a) => a -> Abac (Res)
adword el = fromEndings adWordEndings el

connective :: (Searchable a) => a -> Abac (Res)
connective el = fromWords connectives el

indexical :: (Searchable a) => a -> Abac (Res)
indexical el = fromWords indexicals el

nominalisation :: (Searchable a) => a -> Abac (Res)
nominalisation el = fromEndings nominalisationEndings el

preposition :: (Searchable a) => a -> Abac (Res)
preposition el = fromWords prepositions el

prepexpr :: (Searchable a) => Int -> a -> Abac (Res) -- Expressions
prepexpr n el = fromExpressions (Expressions $ expressionsWith n prepositions el) el

--wrapped version
prepexpr' :: (Searchable a) => NumParam -> a -> Abac (Res) -- Expressions
prepexpr' n' el =
  let NumParam n = n' in fromExpressions (Expressions $ expressionsWith n prepositions el) el



weakverb :: (Searchable a) => a -> Abac (Res)
weakverb el = fromWords weakVerbs el

passives :: (Searchable a) => a -> Abac (Res) -- Expressions
passives el = fromExpressions (Expressions $ passiveExps el) el

passiveExps :: (Searchable a) => a -> [Expression]
passiveExps el = regularPassives el ++ irregularPassives
  where
    regularPassives el' = [ Expression [fst tup, snd tup] | tup <-
      [(vb,part) | vb <- auxiliaries, part <- regularPastParts el' ]]
    regularPastParts el' = wordsWith ed el' where ed = [Word [] (0,0) $ T.pack "ed"]

    irregularPassives = [ Expression [fst tup, snd tup] | tup <-
      [(vb, part) | vb <- auxiliaries, part <- irregularPastParts ]]
    irregularPastParts = pastpartKW defaultKeywords

    auxiliaries = auxiliarKW defaultKeywords :: [Word]

-- ngrams containing passives
passiveNgrams :: (Searchable a) => Int -> a -> Abac Res
passiveNgrams n el =
   let exps = [ ngrm | ngrm <- ngramsIn n el, any (\psv -> ngrm `contains` psv) (passiveExps el) ]
   in  fromExpressions (Expressions exps) el


-- n = n of ngram, m = number of target words that an expression has to contain
prepexps :: (Searchable a) => Int -> Int -> a -> Abac (Res) -- Expressions
prepexps n m el = fromExpressions (Expressions $ multipreps el) el
  where
    nonoverlapping = noOverlapOf' 2 prepositions
    multipreps el' = nonoverlapping $ expressionsWithInExprs m prepositions $ ngramsIn n el'

-- generic functions that produce filters

fromFilter :: (Searchable a) => (Inline -> Bool) -> a -> Abac (Res)
fromFilter fltr el =
  let inls = filter fltr (gatherInlines el)
  in  return $ Res (ListResult $ Inlines inls)
                   (CountResult $ countWordsEqualTo inls el)
                   (PercentResult $ proportionWordsEqualTo inls el)

fromEndings :: (Searchable a) => [Suffix] -> a -> Abac (Res)
fromEndings ends el = return $ Res
  (ListResult $ Inlines $ wordsWith ends el)
  (CountResult $ countWordsWith ends el)
  (PercentResult $ proportionWordsWith ends el)

fromWords :: (Searchable a) => [Word] -> a -> Abac (Res)
fromWords wrds el = return $ Res
  (ListResult $ Inlines $ wordsEqualTo wrds el)
  (CountResult $ countWordsEqualTo wrds el)
  (PercentResult $ proportionWordsEqualTo wrds el)

fromExpressions :: (Searchable a) => Expressions -> a -> Abac (Res) -- Expressions
fromExpressions eexs el =
  let Expressions exs = eexs
  in return $ Res
     (ListResult $ Expressions $ expressionsEqualTo exs el)
     (CountResult $ countExpressions exs el)
     (PercentResult $ proportionExpressions exs el)

-- | alternatives to fromFilter etc. that work outside the Abac monad
---------------------------------------------------------------------

fromFilter' :: Searchable a => (Inline -> Bool) -> a -> (a,Res)
fromFilter' fltr el =
  let inls = filter fltr (gatherInlines el)
  in  (el, Res
        (ListResult $ Inlines inls)
        (CountResult $ countWordsEqualTo inls el)
        (PercentResult $ proportionWordsEqualTo inls el))

fromEndings' :: (Searchable a) => [Suffix] -> a -> (a,Res)
fromEndings' ends el = (el, Res
  (ListResult $ Inlines $ wordsWith ends el)
  (CountResult $ countWordsWith ends el)
  (PercentResult $ proportionWordsWith ends el))

fromWords' :: (Searchable a) => [Word] -> a -> (a,Res)
fromWords' wrds el = (el, Res
  (ListResult $ Inlines $ wordsEqualTo wrds el)
  (CountResult $ countWordsEqualTo wrds el)
  (PercentResult $ proportionWordsEqualTo wrds el))

fromExpressions' :: (Searchable a) => Expressions -> a -> (a,Res) -- Expressions
fromExpressions' eexs el =
  let Expressions exs = eexs
  in (el, Res
       (ListResult $ Expressions $ expressionsEqualTo exs el)
       (CountResult $ countExpressions exs el)
       (PercentResult $ proportionExpressions exs el))

-- accessors

getTree :: (Searchable a) => (a,Res) -> a
getTree (tr,_) = tr

getRes :: (Searchable a) => (a,Res) -> Res
getRes (_,res) = res


-- positional filters

line' :: (Searchable a) => Int -> a -> Inlines
line' n el =
  let originals = M.filter (/= Inlines []) $ Inlines <$> wordsAtLines el
      relatives = mapit $ M.elems originals
      minls = M.lookup n relatives
      --msg = Signal $ T.pack $ "Can't find the line number " ++ show n
  in  case minls of --Abc $ return $ note msg minls
        Just inls -> inls
        Nothing   -> error $ "Can't find the line number " ++ show n

section' :: forall a. (Searchable a) => Text -> a -> Section
section' txtkey el =
  let secmp = mapSections . allSections . gatherSections $ el
      --msg = Signal $ T.append (T.pack "Can't find the section ") txtkey
  in  case (M.lookup txtkey secmp) of --Abc $ return $ note msg $ M.lookup txtkey $ secmp
        Just sc -> sc
        Nothing -> error $ "section': there is no section " ++ T.unpack txtkey

paragraph' :: forall a. (Searchable a) => Int -> a -> Paragraph
paragraph' n el =
  let mpar = M.lookup n $ mapit $ gatherBlocks el
      --msg = Signal $ T.pack $ "there is no paragraph numbered " ++ show n
  in  case mpar of --Abc $ return $ note msg mpar
        Just pr -> pr
        Nothing -> error $ "there is no paragraph numbered " ++ show n




-- end of alternatives
----------------------

-- parts of speech
adWordEndings :: [Suffix]
adWordEndings = adwordKW defaultKeywords

connectives :: [Word]
connectives = connectiveKW defaultKeywords

indexicals :: [Word]
indexicals = indexicalKW defaultKeywords

nominalisationEndings :: [Suffix]
nominalisationEndings = abstractKW defaultKeywords

--passiveVPs :: Document -> [Expression]
--passiveVPs doc = nub $ passiveIrregVPs ++ passiveRegularVPs
--  where
--    passiveIrregVPs, passiveRegularVPs :: [Expression]
--
--    passiveRegularVPs = [ Expression [fst tup, snd tup] | tup <-
--      [(vb,part) | vb <- auxiliaries, part <- regularPastParts doc ]]
--
--    passiveIrregVPs = [ Expression [fst tup, snd tup] | tup <-
--      [(vb, part) | vb <- auxiliaries, part <- irregularPastParts ]]
--
--auxiliaries :: [Word]
--auxiliaries = auxiliarKW defaultKeywords
--
--regularPastParts :: Document -> [Word]
--regularPastParts doc = wordsWithInDocument ed doc
--  where ed = [Word [] (0,0) $ T.pack "ed"]
--
--irregularPastParts :: [Word]
--irregularPastParts = pastpartKW defaultKeywords


prepositions :: [Word]
prepositions = prepositionKW defaultKeywords

weakVerbs :: [Word]
weakVerbs = verbweakKW defaultKeywords


