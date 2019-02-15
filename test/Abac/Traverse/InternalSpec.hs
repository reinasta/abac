{-# LANGUAGE OverloadedStrings #-}
module Abac.Traverse.InternalSpec where

import Text.Megaparsec (runParserT)
import Test.Hspec hiding (Example)

import qualified Abac.Parser as P
import Abac.Parser.Internal (withoutAbbreviations')
import Abac.Internal (mkWords)
import Abac.Types
import Abac.Traverse
import Abac.Traverse.InternalInternal ((%>))

spec :: Spec
spec = describe "Traverse.Internal module" $ do

  it "A sentence of five words" $ do
    Right el <- ptwords1
    countWordsIn (el :: Document) `shouldBe` (5 :: Int)

  it "One string of six tiny words.\n\n " $ do
    Right el <- ptwords2
    countWordsIn (el :: Document) `shouldBe` (6 :: Int)

  it "Get the position of the second word" $ do
    Right el <- ptwords1
    let sent = head (gatherParaParts el)
    let wrds = gatherInlines sent
    let correctPosition = ((1,2),(1,2)) :: (Position, Position)
    getPosition (wrds !! 2) `shouldBe` correctPosition
    -- note: second inline is a Space; the third is a Word (at index 2)

  it "Inlines that are not word-like (e.g. Spaces) are assigned a meaningless, default position" $ do
    Right el <- ptwords1
    let sent = head (gatherParaParts el)
    let wrds = gatherInlines sent
    let correctPosition = ((0,0),(0,0)) :: (Position, Position)
    getPosition (wrds !! 1) `shouldBe` correctPosition
    -- note: second inline (at index 1) is a Space

  it "Get the position of a sentence" $ do
    Right el <- ptwords1
    let sent = head (gatherParaParts el)
    let correctPosition = ((1,0),(1,24)) :: (Position, Position)
    getPosition sent `shouldBe` correctPosition
    -- the end position (24) is read off the last word-like inline.

  it "Get the position of another sentence" $ do
    Right el <- ptwords2
    let sent = head (gatherParaParts el)
    let correctPosition = ((1,0),(1,27)) :: (Position, Position)
    getPosition sent `shouldBe` correctPosition
    -- the end position (27) is read off the last word-like inline.

  it "A five-word sentence generates two 4-grams" $ do
    Right el <- ptwords1
    length (ngramsIn 4 el) `shouldBe` (2 :: Int)

  it "A six-word sentence generates five 2-grams" $ do
    Right el <- ptwords2
    length (ngramsIn 2 el) `shouldBe` (5 :: Int)

  it "One occurrence of the word <six>" $ do
    Right el <- ptwords2
    let expressions = ([Expression [Word [None] (0,0) "six"]] :: [Expression])
    countExpressions expressions el `shouldBe` (1 :: Int)

  it "One occurrence of the expression <five words>" $ do
    Right el <- ptwords1
    let expressions =
          ([Expression [Word [None] (0,0) "five", Word [None] (0,0) "words"]])
    countExpressions expressions el `shouldBe` (1 :: Int)

  it "Proportion of expressions equal to <sentence of> or <five words> in the total number of expressions that can be generated from a sentence" $ do
    Right el <- ptwords1
    let expression1 = Expression [Word [None] (0,0) "sentence", Word [None] (0,0) "of"]
    let expression2 = Expression [Word [None] (0,0) "five", Word [None] (0,0) "words"]
    let expressions = [expression1, expression2]
    proportionExpressions expressions el `shouldBe` (2 %> 4 :: Double)

  it "Total number of inlines in a document" $ do
    Right el <- ptwords2
    length (gatherInlines el) `shouldBe` (13 :: Int)


twords1 = "A sentence of five words. "
twords2 = "One string of six tiny words.\n\n "

ptwords1 = runParserT P.doc "" twords1
ptwords2 = runParserT P.doc "" twords2

--test
adwords_tagged1 :: [Inline]
adwords_tagged1 = taggedWordsInBlock ends pnlptag1_result
  where
    ends = mkWords
      ["able", "ac", "al", "ant", "ary", "ent", "ful",
       "ible", "ic", "ive", "less", "ous"] :: [Suffix]


pnlptag1_result :: Block
pnlptag1_result = Para [Sentence [Word [Tag "IN",None] (1,0) "In",Space,Word [Tag "DT",None] (1,3) "the",Space,Word [Tag "NN",None] (1,7) "middle",Space,Word [Tag "IN",None] (1,14) "of",Space,Word [Tag "PDT",None] (1,17) "all",Space,Word [Tag "DT",None] (1,21) "this",Space,Word [Tag "NN",None] (1,26) "hullabaloo",Punct InSentence ",",Space,Word [Tag "IN",None] (1,38) "after",Space,Word [Tag "DT",None] (1,44) "all",Space,Word [Tag "NN",None] (1,48) "that",Space,Word [Tag "NN",None] (1,53) "struggle",Punct InSentence ",",Space,Word [Tag "PRP",None] (1,63) "I",Space,Word [Tag "VBD",None] (1,65) "was",Space,Word [Tag "VBN",None] (1,69) "horrified",Punct EndSentence ".",Space],Sentence [Word [Tag "NN",None] (1,80) "Sort",Newline,Word [Tag "IN",None] (2,0) "of",Punct EndSentence ".",Space],Sentence [Word [Tag "DT",None] (2,4) "The",Space,Word [Tag "NN",None] (2,8) "merchandise",Space,Word [Tag "VBZ",None] (2,20) "has",Space,Word [Tag "VBN",None] (2,24) "been",Space,Word [Tag "VBN",None] (2,29) "returned",Space,Word [Tag "IN",None] (2,38) "in",Space,Word [Tag "DT",None] (2,41) "the",Space,Word [Tag "NN",None] (2,45) "depot",Space,Word [Tag "IN",None] (2,51) "of",Space,Word [Tag "NNP",None] (2,54) "Berlin",Space,Word [Tag "VBD",None] (2,61) "untouched",Punct InSentence ",",Space,Word [Tag "WDT",None] (2,72) "which",Space,Word [Tag "VBZ",None] (2,78) "is",Space,Word [Tag "JJ",None] (2,81) "abominable",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (2,93) "It",Space,Word [Tag "VBD",None] (2,96) "was",Space,Word [Tag "RB",None] (2,100) "badly",Space,Word [Tag "VBN",None] (2,106) "used",Punct EndSentence ".",Space],Sentence [Word [Tag "NOTAG",None] (2,112) "In",Newline,Word [Tag "NOTAG",None] (3,0) "the",Space,Word [Tag "NOTAG",None] (3,4) "depot",Space,Word [Tag "NOTAG",None] (3,10) "in",Space,Word [Tag "NOTAG",None] (3,13) "Munich",Space,Word [Tag "NOTAG",None] (3,20) "things",Space,Word [Tag "NOTAG",None] (3,27) "were",Space,Word [Tag "NOTAG",None] (3,32) "even",Space,Word [Tag "NOTAG",None] (3,37) "worse",Punct InSentence ":",Space,Word [Tag "NOTAG",None] (3,44) "there",Space,Word [Tag "NOTAG",None] (3,50) "were",Space,Word [Tag "NOTAG",None] (3,55) "people",Space,Word [Tag "NOTAG",None] (3,62) "who",Space,Word [Tag "NOTAG",None] (3,66) "were",Space,Word [Tag "NOTAG",None] (3,71) "being",Space,Word [Tag "NOTAG",None] (3,77) "punished",Space,Word [Tag "NOTAG",None] (3,86) "for",Space,Word [Tag "NOTAG",None] (3,90) "eating",Space,Word [Tag "NOTAG",None] (3,97) "the",Space,Word [Tag "NOTAG",None] (3,101) "only",Space,Word [Tag "NOTAG",None] (3,106) "remaining",Space,Word [Tag "NOTAG",None] (3,116) "food",Space,Word [Tag "NOTAG",None] (3,121) "supplies",Punct EndSentence ".",Null],Sentence [Word [Tag "DT",None] (4,0) "That",Space,Word [Tag "VBD",None] (4,5) "was",Space,Word [Tag "JJ",None] (4,9) "unacceptable",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (4,23) "It",Space,Word [Tag "VBD",None] (4,26) "was",Space,Word [Tag "JJ",None] (4,30) "egregious",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (4,41) "I",Space,Word [Tag "VBD",None] (4,43) "was",Space,Word [Tag "RB",None] (4,47) "really",Space,Word [Tag "VBN",None] (4,54) "horrified",Punct EndSentence ".",Space],Sentence [Word [Tag "PRP",None] (4,65) "I",Newline,Space,Word [Tag "VBD",None] (5,1) "was",Space,Word [Tag "VBN",None] (5,5) "flabbergasted",Punct EndSentence ".",Space]]

--tests: looking for inline having various tags
determiners1 :: [Inline]
determiners1 = determinersInBlock pnlptag1_result :: [Inline]

pastParticiples1 :: [Inline]
pastParticiples1 = pastParticiplesInBlock pnlptag1_result :: [Inline]

adjectives1 :: [Inline]
adjectives1 = adjectivesInBlock pnlptag1_result :: [Inline]

adverbs1 :: [Inline]
adverbs1 = adverbsInBlock pnlptag1_result :: [Inline]


-- toc gives embarassing empty list error on docs/examples/alice.md
--palice :: IO (Either (ParseError Char Void) Document)
palice1 = do
  txt <- readFile "docs/examples/alice.md"
  Right doc <- runParserT P.doc "" (withoutAbbreviations' txt)
  putStrLn $ show $ getAllSections doc

