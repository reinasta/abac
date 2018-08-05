{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.YamlSpec where

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

  describe "Yaml parser:" $ do

    it "This is a yaml (meta) division." $ do
     parseRes <- (fmap . fmap) isMeta pyaml1
     parseRes `shouldBe` Right True

    it "This is another yaml (meta) division" $ do
     parseRes <- (fmap . fmap) isMeta pyaml2
     parseRes `shouldBe` Right True

    it "Yet another yaml (meta) division" $ do
     parseRes <- (fmap . fmap) isMeta pyaml3
     parseRes `shouldBe` Right True

-- Yaml parser --
-----------------

yaml1 = "---\ntitle:  'This is the title: it contains a colon'\nauthor:\n- Author One\n- Author Two\ntags: [nothing, nothingness]\nabstract: |\n  This is the abstract.\n\n  It consists of two paragraphs.\n..."
pyaml1 = runParserT yaml "" yaml1

yaml2 = "---\ntitle:  This is the title without a colon; but it does contain a full stop.\nauthor:\n- Author One\n- Author Two\n- Author Three\ntags: [blah, blah]\nabstract: |\n  This is an abstract that consists of one paragraph.\n\nIn fact, it consists of two paragraphs. The second paragraph ends right here.\n..."
pyaml2 = runParserT yaml "" yaml2

yaml3 = "---\ntitle: This is the title without a colon; but it does contain a full stop.\nauthor:\n- Author One\n- Author Two\n- Author Three\nabstract: |\n  This is an abstract that consists of one paragraph.\n\nIn fact, it consists of two paragraphs. The second paragraph ends right here.\n\n  And a third paragraph, what about that?\ntags: [blah, blah]\n..."
pyaml3 = runParserT yaml "" yaml3

