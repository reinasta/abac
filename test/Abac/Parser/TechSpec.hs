{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.TechSpec where

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
import Abac.Types
import Abac.Traverse
import Abac.Cli.Command
import Abac.Types.ParserTypes
import Abac.Parser
import Abac.Parser.Internal


spec :: Spec
spec = do

  describe "Tech parsers:" $ do

    it "A parser for math blocks" $ do
      parseRes <- ptech1
      parseRes `shouldBe` rtech1

    it "The second parser for math blocks" $ do
      parseRes <- ptech2
      parseRes `shouldBe` rtech2

    it "The third parser for math blocks" $ do
      parseRes <- ptech3
      parseRes `shouldBe` rtech3

    it "The fourth parser for math blocks" $ do
      parseRes <- ptech4
      parseRes `shouldBe` rtech4

    it "The fifth parser for math blocks (with mboxes and escaped closing markers)" $ do
      parseRes <- ptech5
      parseRes `shouldBe` rtech5

    it "The sixth parser for math blocks (with mboxes and escaped closing markers)" $ do
      parseRes <- ptech6
      parseRes `shouldBe` rtech6

    it "The seventh parser for math blocks (with mboxes and escaped closing markers)" $ do
      parseRes <- ptech7
      parseRes `shouldBe` rtech7

    it "The eighth parser for math blocks (with mboxes and escaped closing markers)" $ do
      parseRes <- ptech8
      parseRes `shouldBe` rtech8

    it "The ninth parser for math blocks (with mboxes and escaped closing markers)" $ do
      parseRes <- ptech9
      parseRes `shouldBe` rtech9


-- Tech --
----------

tech1 = "\\( 23 + 55^4 \\$ \\land N_y\\)"
tech2 = "$ 23 + 55^4 \\$ \\land N_y$"
tech3 = "\\( 23 + 55^4 \\$ \\land N_y\\)"
tech4 = "\\( 23 + 55^4 \\\\) \\$ \\land N_y  \\)"

ptech1 = runParserT (mathString openParen closeParen) "" tech1
ptech2 = runParserT (mathString dollar dollar) "" tech2
ptech3 = runParserT (mathString openParen closeParen) "" tech3
ptech4 = runParserT (mathString openParen closeParen) "" tech4

rtech1 = Right "23 + 55^4 \\$ \\land N_y"
rtech2 = Right "23 + 55^4 \\$ \\land N_y"
rtech3 = Right "23 + 55^4 \\$ \\land N_y"
rtech4 = Right "23 + 55^4 \\\\) \\$ \\land N_y  "

-- Math with mbox and escaped closing markers

tech5 = "\\( 23 + 55^4 \\$ \\land N_y\\)"
tech6 = "\\( 23 + 55^4 \\\\) \\$ \\land N_y  \\)"

ptech5 = runParserT (mathString openParen closeParen) "" tech5
ptech6 = runParserT (mathString openParen closeParen) "" tech6

rtech5 = Right "23 + 55^4 \\$ \\land N_y"
rtech6 = Right "23 + 55^4 \\\\) \\$ \\land N_y  "


tech7 = "\\( 23 + 55^4 \\mbox { blah blah \\) blah $ stop } \\\\) \\$ \\land N_y\\)"
tech8 = "\\( 23 + 55^4 \\mbox { blah blah \\) blah $ stop } \\\\) \\mathbox{sec} \\$ \\land N_y\\)"
tech9 = "\\( 23 + 55^4 \\mbox { blah blah \\) blah $ stop } \\mbox {second mbox} \\mbox{third mbox} \\\\) \\mathbox{sec} \\$ \\land \\\\) \\mbox{nth mbox } N_y\\)"


ptech7 = runParserT (mathString openParen closeParen) "" tech7
ptech8 = runParserT (mathString openParen closeParen) "" tech8
ptech9 = runParserT (mathString openParen closeParen) "" tech9

rtech7 = Right "23 + 55^4 \\mbox{blah blah \\) blah $ stop }\\\\) \\$ \\land N_y"
rtech8 = Right "23 + 55^4 \\mbox{blah blah \\) blah $ stop }\\\\) \\mathbox{sec} \\$ \\land N_y"
rtech9 = Right "23 + 55^4 \\mbox{blah blah \\) blah $ stop }\\mbox{second mbox}\\mbox{third mbox}\\\\) \\mathbox{sec} \\$ \\land \\\\) \\mbox{nth mbox }N_y"

