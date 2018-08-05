{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.MarkersSpec where

import Test.Hspec hiding (Example)
import Test.QuickCheck

import Data.List
import Data.Either (isRight,isLeft)
import Data.Either.Combinators (fromRight)
import qualified Data.Text as T
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Test.Hspec.Megaparsec
import Data.Void (Void)
import Prelude hiding (Word)

import qualified NLP.POS as C
import qualified NLP.Tokenize.Chatter as C
import qualified NLP.Types.Tags as C
import qualified NLP.POS.LiteralTagger as C
import qualified NLP.Chunk as C



import Abac.Builder
import Abac.Internal
import Abac.Types
import Abac.Traverse
import Abac.Cli.Command
import Abac.Types.ParserTypes
import Abac.Parser
import Abac.Parser.Internal


spec :: Spec
spec = do

  describe "Marker parsers:" $ do
    let dummy = ItMark 0 zeros "dummy"
    let isExMarker' v = isRight v && (isExMarker . fromRight dummy ) v

    it "A numbered example marker (viz. parser for example items)" $ do
      exm1 >>= (`shouldSatisfy` isExMarker')

    it "Another numbered example marker" $
      exm2 >>= (`shouldSatisfy` isExMarker')

    it "The third numbered example marker" $
      exm3 >>= (`shouldSatisfy` isExMarker')

    it "The fourth numbered example marker" $
      exm4 >>= (`shouldSatisfy` isExMarker')

    it "The fifth numbered example marker" $
      exm5 >>= (`shouldSatisfy` isExMarker')

    it "A @-labeled example marker" $
      exm6 >>= (`shouldSatisfy` isExMarker')

    it "A @-labeled example marker" $
      exm7 >>= (`shouldSatisfy` isExMarker')

    it "A @-labeled example marker" $
      exm8 >>= (`shouldSatisfy` isExMarker')


    it "A marker for a sub-example (marker of level > 0)" $ do
      psubmrk1 >>= (`shouldSatisfy` isRight)

    it "A marker for a sub-example (marker of level > 0)" $ do
      psubmrk2 >>= (`shouldSatisfy` isRight)

    it "A marker for a sub-example (marker of level > 0)" $ do
      psubmrk3 >>= (`shouldSatisfy` isRight)

    it "The sub-example marker parser should fail for level 0 markers" $ do
      psubmrk4 >>= (`shouldSatisfy` isLeft)

    it "The sub-example marker parser should fail here as well" $ do
      psubmrk5 >>= (`shouldSatisfy` isLeft)

    it "Finally, the sub-example marker parser fails here too" $ do
      psubmrk6 >>= (`shouldSatisfy` isLeft)


-- Marker parsers --
--------------------

exm1 = runParserT exmrk "" "\n  12345."
exm2 = runParserT exmrk "" "\n    (12345) "
exm3 = runParserT exmrk "" "\n12345) "
exm4 = runParserT exmrk "" "\n      12345. "
exm5 = runParserT exmrk "" "\n        12345)"
exm6 = runParserT exmrk "" "\n(@ex1) "
exm7 = runParserT exmrk "" "\n  (@ex2) "
exm8 = runParserT exmrk "" "\n   (@ex3) "
exm9 = runParserT exmrk "" "\n(@) "

itm1 = runParserT itmrk "" "\n  *"
itm2 = runParserT itmrk "" "\n    + "
itm3 = runParserT itmrk "" "\n- "
itm4 = runParserT itmrk "" "\n      - "
itm5 = runParserT itmrk "" "\n        +"
itm6 = runParserT itmrk "" "\n* "
itm7 = runParserT itmrk "" "\n  - "
itm8 = runParserT itmrk "" "\n   - "

mrk1 :: IO (Either (ParseError Char Void) Marker)
mrk1 = runParserT marker "" "\n  a. "
mrk2 = runParserT marker "" "\nb) "
mrk3 = runParserT marker "" "\nc."
mrk4 = runParserT marker "" "\ndc)"
mrk5 = runParserT marker "" "\n        ii."
mrk6 = runParserT marker "" "\n    iii)"
mrk7 = runParserT marker "" "\n1. a."
mrk8 = runParserT marker "" "\n1. + "
mrk9 = runParserT marker "" "\n + "
mrk10 = runParserT marker "" "\n(@3) "
mrk11 = runParserT marker "" "\n      (@blah)"
mrk12 = runParserT marker "" "\n      * "
mrk13 = runParserT marker "" "\n    - "

-- sub-example markers

psubmrk1 = runParserT submrk "" "\n  (@blah) "
psubmrk2 = runParserT submrk "" "\n    + "
psubmrk3 = runParserT submrk "" "\n  5) "
psubmrk4 = runParserT submrk "" "\n (@blah) "
psubmrk5 = runParserT submrk "" "\n + "
psubmrk6 = runParserT submrk "" "\n(@5) "

