{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.SectionsSpec where

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
  describe "Section parser:" $ do

   it "Section marker of level 3" $ do
     parseRes <- psecmrk1
     parseRes `shouldBe` Right (SecMark (3 :: Int))

   it "Section marker of level 4" $ do
     psecmrk2 >>= (`shouldBe` Right (SecMark 4))

   it "Section marker of level 1" $ do
     psecmrk3 >>= (`shouldBe` Right (SecMark 1))

   it "This a bare section (that is, without subsections)" $ do
     pbsec1 >>= (`shouldSatisfy` isRight)

   it "Another bare section" $ do
     pbsec2 >>= (`shouldSatisfy` isRight)

   it "The last bare section" $ do
     pbsec3 >>= (`shouldSatisfy` isRight)

   it "A suite of bare sections" $ do
     secTextParser1 >>= (`shouldSatisfy` isRight)

   it "Another suite of bare sections" $ do
     secTextParser2 >>= (`shouldSatisfy` isRight)

   it "Last suite of bare sections" $ do
     secTextParser3 >>= (`shouldSatisfy` isRight)



   it "Embed section of level 2 in section of level 1" $ do
     test_embedSec1Sec2 == True

   it "Embed elements of a list consisting of a pure division and a section of level 1" $ do
     test_embedSec1Div == True

   it "Are sections embedded correctly (wrt their levels)?" $ do
     test_secTextParser1 >>= (`shouldBe` True)

   it "Are these other sections embedded correctly?" $ do
     test_secTextParser2 >>= (`shouldBe` True)

   it "At last, are these other sections embedded correctly?" $ do
     test_secTextParser3 >>= (`shouldBe` True)


   it "A doc with a suite of bare sections" $ do
     secTextParser1 >>= (`shouldSatisfy` isRight)

   it "Another doc with a suite of bare sections" $ do
     secTextParser2 >>= (`shouldSatisfy` isRight)

   it "Last doc with a suite of bare sections" $ do
     secTextParser3 >>= (`shouldSatisfy` isRight)


   it "Testing how section numbers are adjusted with the `fillListWith1s` function" $ do
     fmap fillListWith1s noss == correctResults

   it "The first section is section 1.1.1" $ do
     (secno . head . getSections <$> chmTextDoc) >>=
       (`shouldBe` ((1,1,1,0,0,0) :: No))

   it "I can extract eight (sub)sections from this text" $ do
     (length . getAllSections <$> secTextDoc) >>= (`shouldBe` (8 :: Int))

   it "This text has two main sections" $ do
     (length . getSections <$> secTextDoc) >>= (`shouldBe` (2 :: Int))

   it "This text has one main section" $ do
     (length . getSections <$> chmTextDoc) >>= (`shouldBe` (1 :: Int))

   it "I can extract two (sub)sections from this text" $ do
     (length . getAllSections <$> chmTextDoc) >>= (`shouldBe` (2 :: Int))

   it "Adding a section increases the section-count with one" $ do
     (length . getAllSections <$> addSectionAndParse) >>= \l1 ->
       (((+1) . length . getAllSections <$> secTextDoc) :: IO Int) >>= \l2 ->
         l1 `shouldBe` l2

   it "Twice the text, twice the number of sections" $ do
     (length . getSections <$> parseTwice chmText) >>= \l1 ->
       ( (*2) . length . getSections <$> parseOnce chmText) >>= \l2 ->
         l1 `shouldBe` l2

   it "Four times the text, four times the number of sections" $ do
     (length . getSections <$> parse4x secText) >>= \l1 ->
       ((*4) . length . getSections <$> parseOnce secText) >>= \l2 ->
         l1 `shouldBe`  l2

    -- problem: (==) is not yet defined over Sections
   --it "Is the section numbering correct?" $ do
   --  secTextParser1 == correctNumbering1

{-
   it "Ideally, the doc parser parses any non-empty string" $ do
     property $
       \txt -> txt /= "" ==> parseOnce txt >>= (`shouldNotBe` Doc [])
-}


-- Section parser --
--------------------

twice :: String -> String
twice txt = txt ++ "\n\n\n" ++ txt
fourTimes txt = twice txt ++ "\n\n\n\n" ++ twice txt
parseTwice txt = fromRight (Doc []) <$> (runParserT doc "" (twice txt))
parse4x txt = fromRight (Doc []) <$> (runParserT doc "" (fourTimes txt))
parseOnce txt = fromRight (Doc []) <$> (runParserT doc "" txt)
additionalSection = "\n\n# Additional section\n A paragraph that ends with an exclamation mark!\n\n"

addSectionAndParse = fromRight (Doc []) <$> (runParserT doc "" $ secText ++ additionalSection)



--section markers

secmrk1 = "### Introduction\n\n\n\n"
secmrk2 = "#### Next section (with a title) \n"
secmrk3 = "# A section with a short title, blah. \n"

psecmrk1, psecmrk2, psecmrk3 :: IO (Either (ParseErrorBundle String Void) Marker)
psecmrk1 = runParserT sectionMarker "" secmrk1
psecmrk2 = runParserT sectionMarker "" secmrk2
psecmrk3 = runParserT sectionMarker "" secmrk3



--bare sections

bsec1 = "# Title 1 0\n\nBlock in section 1.0.\n\n"
bsec2 = "###Title 1 3 2\n\nThis is another paragraph!^[Here I sneak a brief remark!]"
bsec3 = "### Introduction\n\n\n\nHe is edging *closer to becoming an octogenarian linguist* and *political activist,* and bound to remember the public birthday wishes he received from his wife Carol when he turned 70: ‘Well, seventy is nice, but what I’m really looking forward to is eighty!’\n\n\nOn his birthday, as on every other day of the year, he receives some 200 e-mails deal-ing with linguistics, politics and other matters.\n\n\nNoam Chomsky is one of the most notable contemporary champions\nof the people. He is also a scientist of the highest calibre. But\nis he great material for a biography? Certainly not, if you ask the\nsubject.\n\n\n"

pbsec1 = runParserT bareSection "" bsec1
pbsec2 = runParserT bareSection "" bsec2
pbsec3 = runParserT bareSection "" bsec3

--text parsed into Document values

chmText = "### Introduction\n\n\n\nHe is edging *closer to becoming an octogenarian linguist* and *political activist,* and bound to remember the public birthday wishes he received from his wife Carol when he turned 70: ‘Well, seventy is nice, but what I’m really looking forward to is eighty!’\n\n\nOn his birthday, as on every other day of the year, he receives some 200 e-mails deal-ing with linguistics, politics and other matters.\n\n\nNoam Chomsky is one of the most notable contemporary champions\nof the people. He is also a scientist of the highest calibre. But\nis he great material for a biography? Certainly not, if you ask the\nsubject.\n\n\n#### Next section with a title\nAn intensely private man, he is horrified to be considered \nthe main character in any story. He jokes about the notion\nthat people come to see him, listen to him, even adore him, when\nin fact he is the most boring speaker ever to hit the stage. He gets\nserious very quickly and tells his audiences that they have come\nto hear about the ‘issues’ of our time, issues that are important to\nthem and, as it happens, to him.\n  \n \nWhat is it that he knows and the\npeople don’t?\n\nWrong question, he would say. The people merely\nwant to know the truth and they know it is hidden from them by a\nvast propaganda machine. His skill is to lift the veil and reveal the\ntruth. Anyone can do it, says Chomsky, it only takes some dedicated\nresearch and logical reasoning."

secText ="# Title 1 0\n\nBlock in section 1.0.\n\n## Title 1 1\n\nBlock in section 1.1.\n\n### Title 1 1 1\n\nBlock in section 1 1.1.\n\n####Title 1 1 1 1\n\nBlock in section 1 1 1.1.\n\n##Title 1 2\n\nBlock in section 1.2.\n\n## Title 1 3\n\nBlock in section 1.3.\n\n###Title 1 3 1\n\nBlock in section 1 3 1.\n\n# Title 2\n\nBlock in section 2."

-- 1 [1.2 [1.2.1 [ 1.2.1.1 ] ] 1.3 [ 1.3.1] ] 2

secText1 ="# Title 1 0\n\nBlock in section 1.0.\n\n## Title 1 1\n\nBlock in section 1.1.\n\n### Title 1 1 1\n\nBlock in section 1 1.1.\n\n####Title 1 1 1 1\n\nBlock in section 1 1 1.1.\n\n##Title 1 2\n\nBlock in section 1.2.\n\n## Title 1 3\n\nBlock in section 1.3.\n\n###Title 1 3 1\n\nBlock in section 1 3 1.\n\n\n####Title 1 3 1 1.\nThis is a paragraph in section 1 3 1 1.\n\n###Title 1 3 2\n\nThis is another paragraph!\n\n# Title 2\n\nBlock in section 2."

emptyDoc = Doc []

secTextDoc = fromRight emptyDoc <$> secTextParser
chmTextDoc = fromRight emptyDoc <$> chmTextParser
secTextParser = runParserT doc "" secText
chmTextParser = runParserT doc "" chmText

secTextParser1 = runParserT sections "" secText
secTextParser2 = runParserT sections "" secText1
secTextParser3 = runParserT sections "" chmText

divNoLev = mkSectionBlocks "Sentence one. Sentence two. Sentence three, blah!/n/nThe second paragraph: this contains only a sentence and ends just here!"
secLev1 = mkSection "2" "The trans-substantiation of the soul" "Hey, this is paragraph one. Hey! It's got three sentences, in total. Actually, four!\n\nThe soul is the essence, indivisible and unchangeable. That's it.\n\n" []


secLev2 = mkSection "2.1" "The meaning of life" "The soul is important. But the life is also.\n\nLife is what the soul experiences until it migrates to the sequel. That's it really!" []


test_embedSec1Div = areSectionsCorrect $ embedAllSections $ divNoLev : secLev2 : []
test_embedSec1Sec2 = areSectionsCorrect $ embedAllSections $ secLev1 : secLev2 : []

-- are these parses correct?


test_secTextParser1 = do { Right divs <- secTextParser1; return $ areSectionsCorrect divs}
test_secTextParser2 = do { Right divs <- secTextParser2; return $ areSectionsCorrect divs}
test_secTextParser3 = do { Right divs <- secTextParser3; return $ areSectionsCorrect divs}

areSectionsCorrect :: [Section] -> Bool
areSectionsCorrect [] = True
areSectionsCorrect (div:divs)
  | noSubs div = True && areSectionsCorrect divs
  | otherwise  = checkSubpartsOf div && areSectionsCorrect divs
  where
    checkSubpartsOf dv@(Section _ _ _ _ _) = foldr (&&) True $ (seclev dv <) . seclev <$> secsbs dv
    checkSubpartsOf _ = True
    noSubs (Section _ _ _ _ subs) = if null subs then True else False
    noSubs _ = True

--correctNumbering1 = Right [Section {secno = (1,0,0,0,0,0), seclev = 1, secttl = Title [Word [None] (1,2) "Title",Space,Word [None] (1,8) "1",Space,Word [None] (1,10) "0"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (3,0) "Block",Space,Word [None] (3,6) "in",Space,Word [None] (3,9) "section",Space,Number [None] (3,17) 1.0,Punct EndSentence ".",Null]]]}, secsbs = [Section {secno = (1,1,0,0,0,0), seclev = 2, secttl = Title [Word [None] (5,3) "Title",Space,Word [None] (5,9) "1",Space,Word [None] (5,11) "1"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (7,0) "Block",Space,Word [None] (7,6) "in",Space,Word [None] (7,9) "section",Space,Number [None] (7,17) 1.1,Punct EndSentence ".",Null]]]}, secsbs = [Section {secno = (1,1,1,0,0,0), seclev = 3, secttl = Title [Word [None] (9,4) "Title",Space,Word [None] (9,10) "1",Space,Word [None] (9,12) "1",Space,Word [None] (9,14) "1"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (11,0) "Block",Space,Word [None] (11,6) "in",Space,Word [None] (11,9) "section",Space,Number [None] (11,17) 1.0,Space,Number [None] (11,19) 1.1,Punct EndSentence ".",Null]]]}, secsbs = [Section {secno = (1,1,1,1,0,0), seclev = 4, secttl = Title [Word [None] (13,4) "Title",Space,Word [None] (13,10) "1",Space,Word [None] (13,12) "1",Space,Word [None] (13,14) "1",Space,Word [None] (13,16) "1"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (15,0) "Block",Space,Word [None] (15,6) "in",Space,Word [None] (15,9) "section",Space,Number [None] (15,17) 1.0,Space,Number [None] (15,19) 1.0,Space,Number [None] (15,21) 1.1,Punct EndSentence ".",Null]]]}, secsbs = []}]}]},Section {secno = (1,2,0,0,0,0), seclev = 2, secttl = Title [Word [None] (17,2) "Title",Space,Word [None] (17,8) "1",Space,Word [None] (17,10) "2"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (19,0) "Block",Space,Word [None] (19,6) "in",Space,Word [None] (19,9) "section",Space,Number [None] (19,17) 1.2,Punct EndSentence ".",Null]]]}, secsbs = []},Section {secno = (1,3,0,0,0,0), seclev = 2, secttl = Title [Word [None] (21,3) "Title",Space,Word [None] (21,9) "1",Space,Word [None] (21,11) "3"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (23,0) "Block",Space,Word [None] (23,6) "in",Space,Word [None] (23,9) "section",Space,Number [None] (23,17) 1.3,Punct EndSentence ".",Null]]]}, secsbs = [Section {secno = (1,3,1,0,0,0), seclev = 3, secttl = Title [Word [None] (25,3) "Title",Space,Word [None] (25,9) "1",Space,Word [None] (25,11) "3",Space,Word [None] (25,13) "1"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (27,0) "Block",Space,Word [None] (27,6) "in",Space,Word [None] (27,9) "section",Space,Number [None] (27,17) 1.0,Space,Number [None] (27,19) 3.0,Space,Number [None] (27,21) 1.0,Punct EndSentence ".",Null]]]}, secsbs = []}]}]},Section {secno = (2,0,0,0,0,0), seclev = 1, secttl = Title [Word [None] (29,2) "Title",Space,Word [None] (29,8) "2"], secbdy = SecBlocks {secblcs = [Para [Sentence [Word [None] (31,0) "Block",Space,Word [None] (31,6) "in",Space,Word [None] (31,9) "section",Space,Number [None] (31,17) 2.0,Punct EndSentence ".",Null]]]}, secsbs = []}]

--fillListWith1s, part of fillWith1s

fillListWith1s :: [Int] -> [Int]
fillListWith1s xs =
  let (zeros, onesAndZeros) = span (== 0) (reverse xs)
      zeroToOne x = if x == 0 then 1 else x
  in  reverse $ zeros ++ fmap zeroToOne onesAndZeros


--Test for `fillListWith1s` which makes up the mechanism that adjusts
--section numbers (type No).

noss = [nos1,nos2,nos3,nos4]
correctResults = [ [1,1,1,0,0,0]
                 , [1,1,1,1,1,0]
                 , [1,1,1,1,1,0]
                 , [1,1,1,1,1,1]
                 ]

nos1, nos2, nos3, nos4 :: [Int]
nos1 = [0,0,1,0,0,0]
nos2 = [0,0,1,0,1,0]
nos3 = [1,0,1,0,1,0]
nos4 = [1,0,1,0,0,1]

