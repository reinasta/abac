{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.InternalSpec where

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
spec = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [1,2,3] `shouldBe` (1 :: Int)



sentHas n attrs einls =
  let Right (Sentence inls) = einls
      words = filter wordlike inls
      targets = traverseWith (`hasAttrs` attrs) (Inlines words)
  in  length targets == n


-- abbreviation test
abrtst1 :: Text
abrtst1 = withoutAbbreviations "I.e. blah viz. blah Mr. Blah Ms. Blah Prof. dr. Blah blah"

--tests
tdd1 = dropdot "pp." " (pp. 34-5, in the Sep. issue and pp. 23-4 in the Oct. issue.)"
tdd2 = dropdot "p." "This is an app."


rplabr1 = replaceAbbreviations abbrevs "Find reference to Mr. X and Dr. Y on pp. 34-5, in the Sep. issue."
rplabr2 = replaceAbbreviations abbrevs "\n-Some text. (pp. 34-5)\n  + Blah."
rplabr3 = replaceAbbreviations abbrevs "\n-Some text. (on pp. 34-5, e.g.)\n  + Blah. viz. pp.\n36-7 i.e. mr. Sep. pp. 12-3. "
rplabr4 = replaceAbbreviations abbrevs "This is an app."


pptest1 = withoutAbbreviations "\n-Some text. (pp. 34-5)\n  + Blah."

