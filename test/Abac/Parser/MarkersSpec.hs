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


    it "First marker for a sub-example (marker of level > 0)" $ do
      psubmrk1 >>= (`shouldSatisfy` isRight)

    it "Second marker for a sub-example (marker of level > 0)" $ do
      psubmrk2 >>= (`shouldSatisfy` isRight)

    it "Third marker for a sub-example (marker of level > 0)" $ do
      psubmrk3 >>= (`shouldSatisfy` isRight)

    it "The sub-example marker parser should fail for level 0 markers" $ do
      psubmrk4 >>= (`shouldSatisfy` isLeft)

    it "The sub-example marker parser should fail here as well" $ do
      psubmrk5 >>= (`shouldSatisfy` isLeft)

    it "Finally, the sub-example marker parser fails here too" $ do
      psubmrk6 >>= (`shouldSatisfy` isLeft)


-- Marker parsers --
--------------------

exm1 = runParserT exmrk "" "  12345."
exm2 = runParserT exmrk "" "    (12345) "
exm3 = runParserT exmrk "" "12345) "
exm4 = runParserT exmrk "" "      12345. "
exm5 = runParserT exmrk "" "        12345)"
exm6 = runParserT exmrk "" "(@ex1) "
exm7 = runParserT exmrk "" "  (@ex2) "
exm8 = runParserT exmrk "" "   (@ex3) "
exm9 = runParserT exmrk "" "(@) "

itm1 = runParserT itmrk "" "\n  *"
itm2 = runParserT itmrk "" "\n    + "
itm3 = runParserT itmrk "" "\n- "
itm4 = runParserT itmrk "" "\n      - "
itm5 = runParserT itmrk "" "\n        +"
itm6 = runParserT itmrk "" "\n* "
itm7 = runParserT itmrk "" "\n  - "
itm8 = runParserT itmrk "" "\n   - "

mrk1 :: IO (Either (ParseErrorBundle String Void) Marker)
mrk1 = runParserT marker "" "  a. "
mrk2 = runParserT marker "" "b) "
mrk3 = runParserT marker "" "c."
mrk4 = runParserT marker "" "dc)"
mrk5 = runParserT marker "" "        ii."
mrk6 = runParserT marker "" "    iii)"
mrk7 = runParserT marker "" "1. a."
mrk8 = runParserT marker "" "1. + "
mrk9 = runParserT marker "" " + "
mrk10 = runParserT marker "" "(@3) "
mrk11 = runParserT marker "" "      (@blah)"
mrk12 = runParserT marker "" "      * "
mrk13 = runParserT marker "" "    - "

-- sub-example markers

psubmrk1 = runParserT submrk "" "\n  (@blah) "
psubmrk2 = runParserT submrk "" "\n    + "
psubmrk3 = runParserT submrk "" "\n  5) "
psubmrk4 = runParserT submrk "" "\n (@blah) "
psubmrk5 = runParserT submrk "" "\n + "
psubmrk6 = runParserT submrk "" "\n(@5) "

