{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.ExamplesSpec where

import Test.Hspec hiding (Example)
import Test.QuickCheck

import Data.List
import Data.Either (isRight,isLeft)
import Data.Either.Combinators (fromRight)
import qualified Data.Text as T
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char (newline)
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

  describe "Example parser:" $ do

    it "This is an (ordered) example item" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso1
      parseRes `shouldBe` Right True

    it "This is another (ordered) example item" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso5
      parseRes `shouldBe` Right True

    it "The third (ordered) example item" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso2
      parseRes `shouldBe` Right True

    it "The fourth (ordered) example item" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso3
      parseRes `shouldBe` Right True

    it "The fifth (ordered) example item, with interspersed newlines" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso4
      parseRes `shouldBe` Right True

    it "This is the marker of an (ordered) example item preceded by a newline" $ do
      parseRes <- pexso1_mrk :: IO (Either (ParseErrorBundle String Void) Marker)
      parseRes `shouldBe` Right (ExMark 0 zeros "2")

    it "This is the main (ordered) example item" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso2
      parseRes `shouldBe` Right True

    it "Two consecutive main (ordered) example items" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso8
      parseRes `shouldBe` Right True

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso9
      parseRes `shouldBe` Right True

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso10
      parseRes `shouldBe` Right True

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso11
      parseRes `shouldBe` Right True

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso12
      parseRes `shouldBe` Right True

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso13
      parseRes `shouldBe` Right True

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso14
      parseRes `shouldBe` Right True

    it "Two sets of ordered examples" $ do
      (fmap . fmap) length pexso15 >>= (`shouldBe` Right (2 :: Int))

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso
      parseRes `shouldBe` Right True

    it "Ordered examples with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pexso1'
      parseRes `shouldBe` Right True

    it "Check whether ordered examples are embedded correctly" $ do
      test_peex1 >>= (`shouldBe` True)

    it "First ordered example without preceding new line" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pex1
      parseRes `shouldBe` Right True

    it "Second ordered example without preceding new line" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pex2
      parseRes `shouldBe` Right True


    it "Third ordered example without preceding new line" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pex3
      parseRes `shouldBe` Right True


    it "Fourth ordered example without preceding new line" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pex4
      parseRes `shouldBe` Right True


    it "Fifth ordered example without preceding new line" $ do
      parseRes <- fmap (all (== True) . fmap isOrdered) <$> pex5
      parseRes `shouldBe` Right True



  describe "Bullet item parser:" $ do

    it "This is an (unordered) bullet item" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pit2
      parseRes `shouldBe` Right True

    it "Another (unordered) bullet item" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pit2
      parseRes `shouldBe` Right True

    it "The third (unordered) bullet item" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pit3
      parseRes `shouldBe` Right True

    it "The fourth (unordered) bullet item" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pit4
      parseRes `shouldBe` Right True

    it "This fifth (unordered) bullet item" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pit5
      parseRes `shouldBe` Right True

    it "Sequence of (unordered) bullet items" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pexsu1
      parseRes `shouldBe` Right True


    it "Sequence of (unordered) bullet items" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pexsu2
      parseRes `shouldBe` Right True

    it "Sequence of (unordered) bullet items" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pexsu3
      parseRes `shouldBe` Right True

    it "Sequence of (unordered) bullet items" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pexsu4
      parseRes `shouldBe` Right True

    it "Several (unordered) bullet items with footnotes" $ do
      parseRes <- fmap (all (== True) . fmap isUnordered) <$> pexsu
      parseRes `shouldBe` Right True

    it "Check whether bullet items are embedded correctly" $ do
      test_peit1 >>= (`shouldBe` True)


  describe "New examples parser:" $ do
    let ordDummy = [ Example Abac.Types.Ordered 0 zeros "dummy" (ExBody []) [] ]
    let unordDummy = [ Example Abac.Types.Unordered 0 zeros "dummy" (ExBody []) [] ]
    let isExample' evs = isRight evs && (all isOrdered . fromRight unordDummy) evs
    let isItem' evs = isRight evs && (all isUnordered . fromRight ordDummy) evs

    it "Parsing many embedded example items" $ do
      peex1 >>= (`shouldSatisfy` isExample')

    it "Parsing many embedded bullet items" $ do
      peit1 >>=  (`shouldSatisfy` isItem')


-- Example parsers --
---------------------

-- ordered

ex1 = "(@blah) This is a sentence. This is also a sentence. What about a question at this point? "
ex2 = "(@23) This is a sentence. This is also a sentence. What about a question at this point? "
ex3 = "(1) This is a sentence. This is also a sentence. What about a question at this point? "
ex4 = "(1) This is a sentence.\n This is also a sentence.\n What about a question at this point? \n  a. Hahaha. One sentence.\n Then another.\n Finally, the last\n sentence."
ex5 = "(@1) This is a sentence. This is also a sentence. What about a question at this point? \n  a. Hahaha. One sentence. Then another. Finally, the last sentence.\n  b. A subexample formed from one single sentence."

pex1 = runParserT examplesNoNewline "" ex1
pex2 = runParserT examplesNoNewline "" ex2
pex3 = runParserT examplesNoNewline "" ex3
pex4 = runParserT examplesNoNewline "" ex4
pex5 = runParserT examplesNoNewline "" ex5



exso1 = "\n(@2) first item\nsecond item\nthird item\nfourth item and end of sentence.\n(@3) Blah."
exso2 = "\n(@3) first item, second item, third item, fourth item and end of sentence.\n(@4) Blah."
exso3 = "\n(@1) Quick sentence.\nfirst item, second item, third item, fourth item and end of sentence.\n(@2) Blah."
exso4 = "\n(@7) Quick\nsentence.\nAnother.\nfirst item, second item, third item, fourth item and end of sentence.\n(@8) Blah."
exso5 = "\n(@8) item\nzero\nAnother item\nfirst item, second item, third item, and last item\n(@2) Blah."
exso6 = "\n(@6) Quick\nsentence.\nAnother one.\nLast\nsentence, blah.\nfirst item, second item, third item, last item\n(@7) Blah."

exso7 = "\n(@4) Quick\nsentence.^[This is a footnote. It spans two sentences.]\nAnother one.\nLast\nsentence, blah.\nfirst item, second item, third item, last item\n^[This is a one\nsentence footnote.]\n(@5) Blah."

exso8 = "\n(@3) Quick sentence.^[This is a footnote. It spans two sentences.]\nAnother one.\nLast\nsentence, blah.\nfirst item, second item, third item, last item\n^[This is a one\nsentence footnote.]\n(@4) Blah."


exso9 = "\n(@4) Quick sentence.^[This is a footnote. It spans two sentences.]\nAnother one.\nLast\nsentence, blah.\n(@5) Blah."


exso10 = "\n(@1) Quick sentence.^[This is a footnote. It spans two sentences.]\nThis sentence.\n^[Finally a footnote!]\n(@2) Blah."


exso11 = "\n(@1) Quick sentence.^[This is a footnote. It spans two sentences.]\nThis sentence.\n^[Finally a footnote!]\n  a. A subordinated item.\n(@2) Blah."
exso12 = "\n(@1) Quick sentence.^[This is a footnote. It spans two sentences.]\nThis sentence.\n^[Finally a footnote!]\n  - A subordinated item.\n  - Subordinated item number two.\n(@2) Blah."

exso13 = "\n(@1) Quick sentence.^[This is\n a footnote.\nIt spans\n two sentences.]\nThis sentence.\n^[Finally a footnote!]\n  - A\nsubordinated\nitem.\n  - Subordinated\nitem number\ntwo.\n(@2) Blah.\n  + a list of items: car, disc, banana\n  + another list: book, computer, blah;\n  c. First sentence! Second sentence. Third sentence, at last!"


exso14 = "\n(@1) Quick sentence.^[This is\n a footnote.\nIt spans\n two sentences.]\nThis sentence.\n^[Finally a footnote!]\n  - A\nsubordinated\nitem.\n  - Subordinated\nitem number\ntwo.\n(@2) Blah.\n  + a list of items: car, disc, banana\n  + another list: book, computer, blah;\n  c. First sentence! Second sentence. Third sentence, at last!\n  + blah,\nblah\nblah.\n\n"

exso15 = "\n(@1) Quick sentence.^[This is\n a footnote.\nIt spans\n two sentences.]\nThis sentence.\n^[Finally a footnote!]\n  - A\nsubordinated\nitem.\n  - Subordinated\nitem number\ntwo.\n(@2) Blah.\n  + a list of items: car, disc, banana\n  + another list: book, computer, blah;\n  c. First sentence! Second sentence. Third sentence, at last!\n  + blah,\nblah\nblah.\n\n(@3) Another hanging example.\n\n  a. With a short sub-example. Really short!\n\n"

exso16 = "\n@ "
exso17 = "\n+"
exso18 = "\n 1."


pexso1 = runParserT examples "" exso1
pexso1_mrk = runParserT (newline *> marker) "" exso1

pexso2 = runParserT examples "" exso2
pexso3 = runParserT examples "" exso3

pexso4 = runParserT examples "" exso4
pexso5 = runParserT examples "" exso5
pexso6 = runParserT examples "" exso6
pexso7 = runParserT examples "" exso7

pexso4' = runParserT marker "" exso4
pexso5' = runParserT marker "" exso5
pexso6' = runParserT marker "" exso6
pexso7' = runParserT marker "" exso7

pexso8 = runParserT examples "" exso8
pexso9 = runParserT examples "" exso9
pexso10 = runParserT examples "" exso10
pexso11 = runParserT examples "" exso11
pexso12 = runParserT examples "" exso12
pexso13 = runParserT examples "" exso13
pexso14 = runParserT examples "" exso14
pexso15 = runParserT examples "" exso15
pexso16 = runParserT (many examples) "" exso16
pexso17 = runParserT (many examples) "" exso17
pexso18 = runParserT (many examples) "" exso18


exso = "\n(@1) first item.\n(@2) second item.^[footnote.]\n(@3) third item.\n  a. fourth item."
pexso = runParserT examples "" exso
exso1' = "\n(@1) first item.\n(@2) second item.^[footnote.]\n(@3) third item.\n  a. fourth item."
pexso1' = runParserT examples "" exso1'


-- unordered

it1 = "\n+ This is a sentence. This is also a sentence. What about a question at this point? "
it2 = "\n- This is a sentence. This is also a sentence. What about a question at this point? "
it3 = "\n+ This is a sentence. This is also a sentence. What about a question at this point? \n  - Hahaha. One sentence. Then another. Finally, the last sentence.\n  - A subexample formed from one single sentence."
it4 = "\n- This is a sentence. This is also a sentence. What about a question at this point? \n  - Hahaha. One sentence. Then another. Finally, the last sentence.\n  - A subexample formed from one single sentence."
it5 = "\n- This is a sentence. This is also a sentence. What about a question at this point? \n  - Hahaha. One sentence. Then another. Finally, the last sentence.\n  - A subexample formed from one single sentence.\n- This is a sentence. This is also a sentence. What about a question at this point? \n  - Hahaha. One sentence. Then another. Finally, the last sentence.\n  - A subexample formed from one single sentence."


pit1 = runParserT examples "" it1
pit2 = runParserT examples "" it2
pit3 = runParserT examples "" it3
pit4 = runParserT examples "" it4
pit5 = runParserT examples "" it5



exsu = "\n- first item.\n- second item.^[footnote.]\n- third item.\n  + fourth item."
exsu1 = "\n- first, and last, item\nwith many\ninterruptions\n- "
exsu2 = "\n- first, and last, item\nwith many\ninterruptions.\n- "
exsu3 = "\n- first, and last, item\nwith many\ninterruptions.\nBut with two sentences\n- "
exsu4 = "\n- first, and penultimate, item\nwith many\ninterruptions.\n  + blah"
pexsu = runParserT examples "" exsu
pexsu1 = runParserT examples "" exsu1
pexsu2 = runParserT examples "" exsu2
pexsu3 = runParserT examples "" exsu3
pexsu4 = runParserT examples "" exsu4



pitm = runParserT examples "" itm
itm = "\n- This is a sentence. This is also a sentence. What about a question at this point? \n  - Hahaha. One sentence. Then another. Finally, the last sentence.\n  - A subexample formed from one single sentence."

-- examples: ordered & unordered --
-----------------------------------

peex1 = runParserT examples "" eex1
peit1 = runParserT examples "" eit1

eex1 ="\n1. Tote bag glossier knausgaard messenger bag put a bird on it.\n  a. Lumbersexual schlitz ramps meh retro? Mlkshk hammock sriracha crucifix fingerstache.^[Remark.] Blah.\n    b. Yuccie health goth venmo iceland pinterest echo park pabst viral deep v brunch +1 twee beard.^[Another remark. A good one.] Blah!\n      i. Green juice drinking vinegar cold-pressed retro twee church-key meh. \n        ii. Iceland cardigan keffiyeh; meh venmo DIY cloud.^[Note this note.] Bread enamel pin paleo echo park pug.\n    c. Blah blah lumbersexual schlitz ramps retro.\n       i. Iceland cardigan keffiyeh. Meh venmo DIY!^[Note!] Cloud bread enamel pin paleo echo park pug.\n         ii. Readymade adaptogen PBR&B locavore hot chicken tattooed umami.^[A short note. Read on!]\n   b. 8-bit poke blog gochujang.^[With a note!] Offal gentrify kombucha etsy lomo ethical hexagon wayfarers gluten-free hot chicken fashion axe roof party vaporware plaid taiyaki.\n2. Coming back to the first level.^[It's been so long, this travel through our subexample-scape!]\n  a. Etsy everyday carry kombucha master cleanse, kickstarter freegan tbh swag hella synth lyft venmo knausgaard fashion axe.^[Note. Short. Like it!]\n  b. Salvia +1 polaroid mlkshk williamsburg. Tacos cronut lo-fi hella sartorial authentic activated charcoal.^[Note: here is the note!] Craft beer portland swag fam skateboard hot chicken fanny pack.\n\n"

eit1 ="\n- Tote bag glossier knausgaard messenger bag put a bird on it.\n   + Lumbersexual schlitz ramps meh retro? Mlkshk hammock sriracha crucifix fingerstache.^[Remark.] Blah.\n     + Yuccie health goth venmo iceland pinterest echo park pabst viral deep v brunch +1 twee beard.^[Another remark. A good one.] Blah!\n       * Green juice drinking vinegar cold-pressed retro twee church-key meh. \n         * Iceland cardigan keffiyeh; meh venmo DIY cloud.^[Note this note.] Bread enamel pin paleo echo park pug.\n     + Blah blah lumbersexual schlitz ramps retro.\n       * Iceland cardigan keffiyeh. Meh venmo DIY!^[Note!] Cloud bread enamel pin paleo echo park pug.\n         * Readymade adaptogen PBR&B locavore hot chicken tattooed umami.^[A short note. Read on!]\n   + 8-bit poke blog gochujang.^[With a note!] Offal gentrify kombucha etsy lomo ethical hexagon wayfarers gluten-free hot chicken fashion axe roof party vaporware plaid taiyaki.\n- Coming back to the first level.^[It's been so long, this travel through our subexample-scape!]\n  + Etsy everyday carry kombucha master cleanse, kickstarter freegan tbh swag hella synth lyft venmo knausgaard fashion axe.^[Note. Short. Like it!]\n  + Salvia +1 polaroid mlkshk williamsburg. Tacos cronut lo-fi hella sartorial authentic activated charcoal.^[Note: here is the note!] Craft beer portland swag fam skateboard hot chicken fanny pack.\n"

test_peex1 = do {Right exs <- peex1; return $ areExamplesCorrect exs}
test_peit1 = do {Right exs <- peit1; return $ areExamplesCorrect exs}

areExamplesCorrect :: [Example] -> Bool
areExamplesCorrect [] = True
areExamplesCorrect (ex:exs)
  | null (exampleSubparts ex) = True && areExamplesCorrect exs
  | otherwise                 = checkSubpartsOf ex && areExamplesCorrect exs
  where
    checkSubpartsOf exmp =
      foldr1 (&&) $ (exampleLevel exmp <) <$> exampleLevel <$> exampleSubparts exmp

-- additional examples (to be added to tests)

rangeNext1 :: [No]
rangeNext1 = take 5 $ genNextSecNoRange (listToTup ints4)

range1 :: [No]
range1 = take 5 $ genSubsecNoRange (listToTup ints3)

ints1,ints2,ints3,ints4 :: [Int]
ints1 = [0,0,0,0,0,0]
ints2 = [1,0,0,0,0,0]
ints3 = [1,1,1,1,0,0]
ints4 = [1,1,1,1,1,1]

