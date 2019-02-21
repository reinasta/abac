module Abac.Traverse.MetaSpec where

import Text.Megaparsec
import Test.Hspec hiding (Example)


import Abac.Parser
import Abac.Traverse


spec :: Spec
spec = describe "Meta info (reading time)" $  do

  it "A 200-word text is read in 1 minute" $ do
    Right doc <- runParserT doc "" mtext_200
    readingTime doc `shouldBe` (1 :: Int)

  it "A 400-word text is read in 2 minutes" $ do
    Right doc <- runParserT doc "" mtext_400
    readingTime doc `shouldBe` (2 :: Int)

  it "A 410-word text is read also in 2 minutes" $ do
    Right doc <- runParserT doc "" (mtext_400 ++ msent_base)
    readingTime doc `shouldBe` (2 :: Int)

  it "More problematically, a 590-word text is read also in 2 minutes" $ do
    Right doc <- runParserT doc "" mtext_590
    readingTime doc `shouldBe` (2 :: Int)


-- a ten-word sentence
msent_base = "one two three four five six seven eight nine ten. "

mtext_200 = concat $ take 20 (repeat msent_base)
mtext_400 = concat $ take 40 (repeat msent_base)
mtext_590 = concat $ take 59 (repeat msent_base)
