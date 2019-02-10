{-# LANGUAGE OverloadedStrings #-}
module Abac.TraverseSpec where

import qualified Data.Text as T (unpack,Text)
import Abac.Parser.Internal (withoutAbbreviations')

import Text.Megaparsec
import Test.Hspec hiding (Example)

import Abac.Traverse
import Abac.Parser

spec :: Spec
spec = describe "Traversing the document structure" $ do

  it "returns the table of contents (toc) of the alice.md document" $ do
    txt <- readFile "docs/examples/alice.md"
    Right dcm <- runParserT doc "" (withoutAbbreviations' txt)
    toc dcm `shouldBe` palice1'_toc


palice1'_toc :: T.Text
palice1'_toc = " Alice’s Adventures In Wonderland (position 0:0, words 0)\n1 Chapter I: Down The Rabbit-hole (position 9:2, words 2146)\n2 Chapter Ii: The Pool Of Tears (position 213:2, words 2113)"
