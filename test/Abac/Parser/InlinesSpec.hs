{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.InlinesSpec where

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
--import qualified Abac.Cli.Command as C
import Abac.Types.ParserTypes
import Abac.Parser
import Abac.Parser.Internal


spec :: Spec
spec = do

  describe "Link parser:" $ do

    it "This stream contains a link" $ do
      parseRes <- fmap (any isLink) <$> plk1
      parseRes `shouldBe` Right True

    it "This stream contains a link" $ do
      parseRes <- fmap (any isLink) <$> plk2
      parseRes `shouldBe` Right True

    it "Two blocks with an inline link and a link reference" $ do
      parseRes <- fmap (any isLink) <$> plk3
      parseRes `shouldBe` Right True

    it "Two other blocks with an inline link and a link reference" $ do
      parseRes <- fmap (any isLinkRef) <$> plk4
      parseRes `shouldBe` Right True



  describe "Image parser:" $ do

    it "This stream contains an image with a reference id" $ do
      parseRes <- fmap (any isImage) pim0
      parseRes `shouldBe` True

    it "This stream contains an image" $ do
      parseRes <- fmap (any isImage) <$> pim1
      parseRes `shouldBe` Right True

    it "This stream contains an image" $ do
      parseRes <- fmap (any isImage) <$> pim2
      parseRes `shouldBe` Right True

    it "An image reference block" $ do
      parseRes <- fmap (any isImageRef) pim3
      parseRes `shouldBe` True

    it "Two blocks with an inline image and an image reference" $ do
      parseRes <- fmap (any isImageRef) <$> pim4
      parseRes `shouldBe` Right True

  describe "Word and punctuation parsers:" $ do
    let wrd = Word [] (0,0)

    it "This is a word-dashes-word sequence" $ do
      parseRes <- fmap length <$> pdsh1
      parseRes `shouldBe` Right (3 :: Int)

    it "Another word-dashes-word sequence" $ do
      parseRes <- fmap length <$> pdsh2
      parseRes `shouldBe` Right (5 :: Int)

    it "A single word with two dashes inside" $ do
      parseRes <- fmap length <$> pdsh3
      parseRes `shouldBe` Right (1 :: Int)

    it "A single word with a dash inside" $ do
      parseRes <- fmap length <$> pdsh4
      parseRes `shouldBe` Right (1 :: Int)

    it "A word with a dash suffix plus two other words" $ do
      parseRes <- fmap length <$> pdsh5
      parseRes `shouldBe` Right (5 :: Int)

    it "A word with a dash suffix plus two other words" $ do
      parseRes <- fmap (length . filter wordlike) <$> pdsh6
      parseRes `shouldBe` Right (11 :: Int)

    it "No inline in here" $ do
      pmultidsh >>= \res -> res `shouldBe` Right []

    -- words with apostrophes
    it "This is a word with a final apostrophe" $ do
      parseRes <- fmap (== wrd "Lewis'") <$> apostrophe1
      parseRes `shouldBe` Right True

    it "The same word with final apostrophe parsed by a more general parser" $ do
      parseRes <- fmap ((== wrd "Lewis'") . head) <$> apostrophe1i
      parseRes `shouldBe` Right True

    it "This is a word ending in \"s\'\"" $ do
      parseRes <- fmap (== wrd "parents'") <$> apostrophe2
      parseRes `shouldBe` Right True

    it "The same word ending in \"s\'\" parsed by a more general parser" $ do
      parseRes <- fmap ((== wrd "parents'") . head) <$> apostrophe2i
      parseRes `shouldBe` Right True

    it "A regular possessive ending in \"\'s\" " $ do
      parseRes <- fmap (== wrd "Lewis's") <$> apostrophe3
      parseRes `shouldBe` Right True

    it "The same regular possessive ending in \"\'s\" parsed by a more general parser" $ do
      parseRes <- fmap ((== wrd "Lewis's") . head) <$> apostrophe3i
      parseRes `shouldBe` Right True

    it "Another regular possessive ending in \"\'s\" " $ do
      parseRes <- fmap (== wrd "brother's") <$> apostrophe4
      parseRes `shouldBe` Right True

    it "The same regular possessive ending in \"\'s\" parsed by a more general parser" $ do
      parseRes <- fmap ((== wrd "brother's") . head) <$> apostrophe4i
      parseRes `shouldBe` Right True

    it "An illegal word ending in an apostrophe (which is not hence parsed) " $ do
      parseRes <- fmap (== wrd "brother") <$> apostrophe5
      parseRes `shouldBe` Right True

    it "The same illegal word ending in an apostrophe, parsed by a more general parser" $ do
      parseRes <- fmap ((== wrd "brother") . head) <$> apostrophe5i
      parseRes `shouldBe` Right True

    it "An illegal word starting with an apostrophe" $ do
      parseRes <- isLeft <$> apostrophe6
      parseRes `shouldBe` True

    it "The same illegal word starting with an apostrophe, parsed by a more general parser" $ do
      apostrophe6i >>= (`shouldBe` Right [])

    it "This is a word with numbers and a final apostrophe" $ do
      parseRes <- fmap (== wrd "90's") <$> apostrophe7
      parseRes `shouldBe` Right True

    it "The same word with numbers and a final apostrophe, parsed by a more general parser" $ do
      parseRes <- fmap ((== wrd "90's") . head) <$> apostrophe7i
      parseRes `shouldBe` Right True

    -- no final apostrophe allowed
    it "This is a word with a final apostrophe" $ do
      parseRes <- fmap (== wrd "Lewis") <$> apostrophe1'
      parseRes `shouldBe` Right True

    it "This is a word ending in \"s\'\"" $ do
      parseRes <- fmap (== wrd "parents") <$> apostrophe2'
      parseRes `shouldBe` Right True

    it "A regular possessive ending in \"\'s\" " $ do
      parseRes <- fmap (== wrd "Lewis's") <$> apostrophe3'
      parseRes `shouldBe` Right True

    it "Another regular possessive ending in \"\'s\" " $ do
      parseRes <- fmap (== wrd "brother's") <$> apostrophe4'
      parseRes `shouldBe` Right True

    it "An illegal word ending in an apostrophe (which is not hence parsed) " $ do
      parseRes <- fmap (== wrd "brother") <$> apostrophe5'
      parseRes `shouldBe` Right True

    it "An illegal word ending in an apostrophe (which is not hence parsed) " $ do
      parseRes <- isLeft <$> apostrophe6'
      parseRes `shouldBe` True

    it "This is a word with numbers and a final apostrophe" $ do
      parseRes <- fmap (== wrd "90's") <$> apostrophe7'
      parseRes `shouldBe` Right True

    it "Apostrophes followed by 's' are included in citation keys" $ do
      parseRes <- fmap (== [Citation [None] 0 (1,1) "russell1904's"]) <$> pcitapo1
      parseRes `shouldBe` Right True


    -- some corner cases
    let
        manyHave' n attrs einls =
          let Right inls = einls
              words = filter wordlike inls
              targets = traverseWith (`hasAttrs` attrs) (Inlines words)
          in  length targets == n

    it "Two quoted words among several others" $ do
      papocorner1 >>= (`shouldSatisfy` manyHave' 2 [Quoted])

    it "No quoted words in this tricky string" $ do
      papocorner2 >>= (`shouldSatisfy` manyHave' 0 [Quoted])

    it "Another tricky string with no quoted words" $ do
      papocorner3 >>= (`shouldSatisfy` manyHave' 5 [Quoted])




  describe "Number parsers:" $ do

    it "Two numbers among several words" $ do
      parseRes <- fmap (length . filter isNumber) <$> pnumvsword1
      parseRes `shouldBe` Right (2 :: Int)

    it "Four numbers among several words" $ do
      parseRes <- fmap (length . filter isNumber) <$> pnumvsword2
      parseRes `shouldBe` Right (4 :: Int)


  describe "Email parsers:" $ do

    it "Simple email address" $ do
      parseRes <- fmap isEmail <$> peml1
      parseRes `shouldBe` Right True

    it "Email address with full stop" $ do
      parseRes <- fmap isEmail <$> peml2
      parseRes `shouldBe` Right True

    it "Email address with underscore" $ do
      parseRes <- fmap isEmail <$> peml3
      parseRes `shouldBe` Right True

    it "One email address among several inlines" $ do
      parseRes <- fmap (length . filter isEmail) <$> peml4
      parseRes `shouldBe` Right (1 :: Int)

    it "Two email addresses among other inlines" $ do
      parseRes <- fmap (length . filter isEmail) <$> peml5
      parseRes `shouldBe` Right (2 :: Int)


  describe "Word styling parsers:" $ do

    it "This sentence has 5 words in italics" $ do
      parseRes <- fmap (length . emph . unwrapParaPart') <$> pemph1
      parseRes `shouldBe` Right (5 :: Int)

    it "This sentence has 5 words in bold" $ do
      parseRes <- fmap (length . bold . unwrapParaPart') <$> pemph1
      parseRes `shouldBe` Right (5 :: Int)

    it "A sequence of 6 words in italics" $ do
      parseRes <- fmap (length . emph) <$> pemph2
      parseRes `shouldBe` Right (6 :: Int)

    it "Another sequence of 6 words in italics" $ do
      parseRes <- fmap (length . emph) <$> pemph3
      parseRes `shouldBe` Right (6 :: Int)

    it "A sequence of 6 words in bold" $ do
      parseRes <- fmap (length . bold) <$> pemph4
      parseRes `shouldBe` Right (6 :: Int)

    it "Another sequence of 6 words in bold" $ do
      parseRes <- fmap (length . bold) <$> pemph5
      parseRes `shouldBe` Right (6 :: Int)

    it "Inner word italics are not accepted" $ do
      parseRes <- fmap null <$> pemph6
      parseRes `shouldBe` Right (True :: Bool)

    it "Inner word bold chars are not accepted" $ do
      parseRes <- fmap null <$> pemph7
      parseRes `shouldBe` Right (True :: Bool)



  describe "Matching punctuation:" $ do
    let --manyHave :: Int -> [Attr] -> Either (ParseError Char Void) [Inline] -> Bool;
        manyHave n attrs einls =
          let Right inls = einls
              words = filter wordlike inls
              targets = traverseWith (`hasAttrs` attrs) (Inlines words)
          in  length targets == n
    let --manyHaveExactly :: Int -> [Attr] -> Either (ParseError Char Void) [Inline] -> Bool;
        manyHaveExactly n attrs einls =
          let Right inls = einls
              words = filter wordlike inls
              targets = traverseWith (`hasExactlyAttrs` attrs) (Inlines words)
          in  length targets == n
    let --manyHave :: Int -> [Attr] -> Either (ParseError Char Void) [Inline] -> Bool;
        sentHas n attrs einls =
          let Right (Sentence inls) = einls
              words = filter wordlike inls
              targets = traverseWith (`hasAttrs` attrs) (Inlines words)
          in  length targets == n


    -- brackets
    it "Some words in brackets" $ do
      pbracks0 >>= (`shouldSatisfy` manyHave 5 [Bracketed])

    it "Five bracketed words in bold" $ do
      pbracks1 >>= (`shouldSatisfy` manyHave 5 [Bracketed,Bold])

    it "Two bracketed words in italics" $ do
      pbracks2 >>= (`shouldSatisfy` manyHave 2 [Bracketed,Emph])

    it "Eight multiply bracketed words" $ do
      pbracks3 >>= (`shouldSatisfy` manyHave 8 [Bracketed])

    it "Again: two bracketed words in italics" $ do
      pbracks4 >>= (`shouldSatisfy` manyHave 2 [Bracketed,Emph])

    it "Unbalanced brackets" $ do
      pbracks5 >>= (`shouldSatisfy` manyHave 0 [Bracketed])

    it "Four bracketed words" $ do
      pbracks6 >>= (`shouldSatisfy` manyHave 4 [Bracketed])

    it "One word which is bracketed, italic and quoted" $ do
      pbracks_dec1 >>= (`shouldSatisfy` manyHave 1 [Bracketed,DoubleQuoted,Emph])

    it "Four words which are bracketed and italic" $ do
      pbracks_dec3 >>= (`shouldSatisfy` manyHave 4 [Bracketed,Emph])

    it "Six words which are bracketed and quoted" $ do
      pbracks_dec3 >>= (`shouldSatisfy` manyHave 6 [Bracketed,DoubleQuoted])

    it "Empty string" $ do
      inlines_empty >>= (`shouldSatisfy` (== Right []))

    it "Quoted space character are skipped by the inline parser" $ do
      inlines_almost_empty1 >>= (`shouldSatisfy` (== Right []))

    it "Empty quoted strings are skipped by the inline parser" $ do
      inlines_almost_empty2 >>= (`shouldSatisfy` (== Right [Word [None] (1,3) "blah"]))



    -- emphasis
    it "Thirteen words in italics; two of them are bracketed" $ do
      let italBrackProp :: Either (ParseError Char Void) [Inline] -> Bool;
          italBrackProp einls = manyHave 13 [Emph] einls && manyHave 2 [Emph, Bracketed] einls
      pemphs1 >>= (`shouldSatisfy` italBrackProp)

    it "Thirteen words in italics" $ do
      pemphs2 >>= (`shouldSatisfy` manyHave 13 [Emph])

    it "Two words in italics" $ do
      pemphs3 >>= (`shouldSatisfy` manyHave 2 [Emph])

    it "Two words in bold" $ do
      styledinls1 >>= (`shouldSatisfy` manyHave 2 [Bold])

    -- single and double quotes
    it "Four single-quoted words" $ do
      pquote1 >>= (`shouldSatisfy` manyHave 4 [Quoted])

    it "Again: four single-quoted words" $ do
      pquote2 >>= (`shouldSatisfy` manyHave 4 [Quoted])

    it "Six single-quoted words" $ do
      pquote3 >>= (`shouldSatisfy` manyHave 6 [Quoted])

    it "Seven single-quoted words" $ do
      pquote4 >>= (`shouldSatisfy` manyHave 7 [Quoted])

    it "Six single-quoted words" $ do
      pquote5 >>= (`shouldSatisfy` manyHave 6 [Quoted])

    it "Again: six single-quoted words" $ do
      pquote6 >>= (`shouldSatisfy` manyHave 6 [Quoted])

    it "Again: six single-quoted words" $ do
      pquote6 >>= (`shouldSatisfy` manyHave 6 [Quoted])

    it "One bracketed, single-quoted word; seven single-quoted ones; two bracketed" $ do
      let quoteBrackProp :: Either (ParseError Char Void) [Inline] -> Bool;
          quoteBrackProp einls =
            manyHave 7 [Quoted] einls
            && manyHave 1 [Quoted, Bracketed] einls
            && manyHaveExactly 2 [Bracketed] einls
      pquote7 >>= (`shouldSatisfy` quoteBrackProp)

    it "Seven single-quoted words; one single-quoted, bracketed word; two bracketed" $ do
      let quoteEmphProp :: Either (ParseError Char Void) [Inline] -> Bool;
          quoteEmphProp einls =
            manyHave 7 [Quoted] einls
            && manyHave 1 [Quoted, Bracketed] einls
            && manyHaveExactly 2 [Bracketed] einls
      pquote8 >>= (`shouldSatisfy` quoteEmphProp)

    it "Seven single-quoted words; one single-quoted, bracketed and italic; two bracketed and parenthetical" $ do
      let quoteEtcProp :: Either (ParseError Char Void) [Inline] -> Bool;
          quoteEtcProp einls =
            manyHave 7 [Quoted] einls
            && manyHave 1 [Quoted, Bracketed,Emph] einls
            && manyHave 2 [Bracketed,Parenthetical] einls
      pquote9 >>= (`shouldSatisfy` quoteEtcProp)

    it "Eleven double-quoted words" $ do
      pquote10 >>= (`shouldSatisfy` manyHave 11 [DoubleQuoted])

    it "One double-quoted and single-quoted word" $ do
      pquote11 >>= (`shouldSatisfy` manyHave 1 [Quoted,DoubleQuoted])

    it "Again: one double-quoted and single-quoted word" $ do
      pquote11 >>= (`shouldSatisfy` manyHave 1 [Quoted,DoubleQuoted])

-- Links & images --
--------------------

-- links

lk1 = "This is [an example](http://example.com/ \"Title\") inline link." :: String
lk2 = "[This link](http://example.net/) has no title attribute." :: String
lk3 = "See my [About](/about/) page for details."
lk4 = "This is [an example] [id] reference-style link.\n\n  [id]: http://example.com/  \"Optional Title Here\""

plk1 = runParserT (many $ try inline) "" lk1
plk2 = runParserT (many $ try inline) "" lk2
plk3 = runParserT (many $ try inline) "" lk3
plk4 = runParserT (many block) "" lk4

id1 = "[id]: http://example.com/  \"Optional Title Here\""
pid1 = runParserT linkref "" id1


-- images

im0 = "![Alt text][id]"
im1 = "An image, ![Alt text](/path/to/img.jpg)."
im2 = "Another image, ![Alt text](/path/to/img.jpg \"Optional title\") blah."
im3 = " [id]: url/to/image  \"Optional title attribute\"\n\n "
im4 = "Here is an image: ![Alt text][id].\n\n [id]: url/to/image  \"Optional title attribute\""

pim0 = runParserT image "" im0
pim1 = runParserT (many $ try inline) "" im1
pim2 = runParserT (many $ try inline) "" im2
pim3 = runParserT block "" im3
pim4 = runParserT (many block) "" im4

pim5 = runParserT (many $ try inline) "" im4


-- Words --
-----------

-- testing strings with dashes (words vs in-sentence punct)

dsh1 = "blah---blah"
dsh2 = "blah -- blah"
dsh3 = "blah--blah"
dsh4 = "blah-blah"
dsh5 = "cross- or inter-disciplinary"
dsh6 = "My research is cross-disciplinary and inter-disciplinary, and I'm proud of it"
dsh7 = "His own flip--flop to flip--flop with"
multidsh = "---- ----- --------"

pdsh1 = runParserT decoratedInlines "" dsh1
pdsh2 = runParserT decoratedInlines "" dsh2
pdsh3 = runParserT decoratedInlines "" dsh3
pdsh4 = runParserT decoratedInlines "" dsh4
pdsh5 = runParserT decoratedInlines "" dsh5
pdsh6 = runParserT decoratedInlines "" dsh6
pdsh7 = runParserT decoratedInlines "" dsh7
pmultidsh = runParserT decoratedInlines "" multidsh


apostrophe1 = runParserT wordWithApostrophe "" "Lewis'"
apostrophe2 = runParserT wordWithApostrophe "" "parents'"
apostrophe3 = runParserT wordWithApostrophe "" "Lewis's"
apostrophe4 = runParserT wordWithApostrophe "" "brother's"
apostrophe5 = runParserT wordWithApostrophe "" "brother'" -- parses the letters up to the apostrophe
apostrophe6 = runParserT wordWithApostrophe "" "'brother" -- should fail
apostrophe7 = runParserT wordWithApostrophe "" "90's"

apostrophe1i = runParserT decoratedInlines "" "Lewis'"
apostrophe2i = runParserT decoratedInlines "" "parents'"
apostrophe3i = runParserT decoratedInlines "" "Lewis's"
apostrophe4i = runParserT decoratedInlines "" "brother's"
apostrophe5i = runParserT decoratedInlines "" "brother'" -- parses the letters up to the apostrophe
apostrophe6i = runParserT decoratedInlines "" "'brother" -- should fail
apostrophe7i = runParserT decoratedInlines "" "90's"



apostrophe1' = runParserT wordSansApostrophe "" "Lewis'" -- parses every letter up to the apostrophe
apostrophe2' = runParserT wordSansApostrophe "" "parents'" -- likewise
apostrophe3' = runParserT wordSansApostrophe "" "Lewis's"
apostrophe4' = runParserT wordSansApostrophe "" "brother's"
apostrophe5' = runParserT wordSansApostrophe "" "brother'" -- likewise
apostrophe6' = runParserT wordSansApostrophe "" "'brother" -- should fail
apostrophe7' = runParserT wordSansApostrophe "" "90's"

pcitapo1 = runParserT decoratedInlines "" "@russell1904's"

apocorner1 = "He said: 'it's Lewis's'"
apocorner2 = "give me 1' or 1'' units"
apocorner3 = "he said 'it measures 1'' at least' "

papocorner1 = runParserT decoratedInlines "" apocorner1
papocorner2 = runParserT decoratedInlines "" apocorner2
papocorner3 = runParserT decoratedInlines "" apocorner3



-- Styling --
-------------

emph1 = "**He** *is* *edging* **closer to** **becoming** an octogenarian *linguist* and *political activist,* and bound to remember the public birthday wishes he received from his **wife** Carol when he turned 70."
emph2 = "*words* *words* *words* *words* *words* *words*"
emph3 = "*words words words words words words*"
emph4 = "**words** **words** **words** **words** **words** **words**"
emph5 = "**words words words words words words**"




pemph1 = runParserT sentence "" emph1
pemph2 = runParserT styledInlines "" emph2
pemph3 = runParserT styledInlines "" emph3
pemph4 = runParserT styledInlines "" emph4
pemph5 = runParserT styledInlines "" emph5


-- inner-word styling: these should fail
pemph6 = runParserT styledInlines "" "*re*apply"
pemph7 = runParserT styledInlines "" "**re**apply"



-- other inlines -- TO DO
-------------------------

-- emails
eml1 = "name@odisee.org"
eml2 = "name.surname@gmail.com"
eml3 = "name_surname@gmail.com"
eml4 = "Write me at surname@gmail.com."
eml5 = "Her email address is pop@gmail.com, or me@pop.org"

peml1 = runParserT email "" eml1
peml2 = runParserT email "" eml2
peml3 = runParserT email "" eml3
peml4 = runParserT decoratedInlines "" eml4
peml5 = runParserT decoratedInlines "" eml5



-- numbers
numvsword1 = "they are 3 and we are twice that, namely 6"
numvsword2 = "2 plus 2 plus 2 equals 6"

pnumvsword1 = runParserT (many inlineSansBalancing') "" numvsword1
pnumvsword2 = runParserT (many inlineSansBalancing') "" numvsword2



-- parentheses etc --
---------------------

-- brackets
pbracks0 = runParserT decoratedInlines "" bracks0
pbracks1 = runParserT decoratedInlines "" bracks1
pbracks2 = runParserT decoratedInlines "" bracks2
pbracks3 = runParserT decoratedInlines "" bracks3
pbracks4 = runParserT decoratedInlines "" bracks4
pbracks5 = runParserT decoratedInlines "" bracks5 -- must fail and return []
pbracks6 = runParserT decoratedInlines "" bracks6
pbracks7 = runParserT decoratedInlines "" "[falsely not \"falsely\"]"
pbracks8 = runParserT decoratedInlines "" "[**bold**]"
pbracks9 = runParserT bracksApostrophe "" "['quoted']"

bracks0 = "[simple words enclosed in brackets]"
bracks1 = "several words [**that are emphasized** with [**bold characters**] within *brackets*]"
bracks2 = "[several words [**that are emphasized** with [**bold characters**] within *brackets*] within even *larger* brackets]"
bracks3 = "[woah, [there are [so many [brackets [in here]]] ]  ]"
bracks4 = "[woah, [there are [so *many* [brackets [in here]]] ] *but* we are not done] yet"
bracks5 = "[woah, [there are [so many [brackets [in here]]] ] but they are unbalanced" -- unbalanced!
bracks6 = "[bracketed words [doubly so]]"


pbracks_dec1 = runParserT decoratedInlines "" brackDoubleEmph1
pbracks_dec2 = runParserT decoratedInlines "" brackDoubleEmph2
pbracks_dec3 = runParserT decoratedInlines "" brackDoubleEmph3

-- emphasis
brackDoubleEmph1 = "[word1 *emph1 \"emph-quote\" emph2* word2] no brackets"
brackDoubleEmph2 = "[word1 *emph1 \"emph-quote\" emph2* word2] *no* brackets at all"
brackDoubleEmph3 = "[word1 *emph1 \"emph-quote\" emph2* word2] [with \"*no* brackets at all\", says she [falsely not \"falsely\"] ]"

styledinls1 = runParserT decoratedInlines "" "**bold characters**]"
inlines_empty = runParserT decoratedInlines "" ""

inlines_almost_empty1, inlines_almost_empty2 :: IO (Either (ParseError Char Void) [Inline])
inlines_almost_empty1 = runParserT decoratedInlines "" "\" \""
inlines_almost_empty2 = runParserT decoratedInlines "" "\"\" blah"

emphs1 = "*emphasized words [bracketed words] unbracketed but emphasized, and *some which are doubly emphasized* *"
emphs2 = "*emphasized words and some *doubly emphasized words*, and again *double italics* blah *hal* *"
emphs3 = "or *double italics* "

pemphs1 = runParserT emphsApostrophe "" emphs1
pemphs2 = runParserT emphsApostrophe "" emphs2
pemphs3 = runParserT decoratedInlines "" emphs3

-- quotes
quote1 = "He said 'Lewis' theory is mistaken' "
quote2 = "He said 'Lewis's theory is mistaken' "
quote3 = "'Of course I'm my mother's son' John said"
quote4 = "He is believed to have said 'The only theory that's mistaken is Lewis' "
quote5 = "'Of course I'm my parents' son' John said"
quote6 = "'Of course I love my parents' John said"


quote7 = "'Of course [cough] I love my parents' [John uttered]"
quote8 = "'*Of course* [cough] I love my parents' [John said]"
quote9 = "'**Of course** [*cough*] I love my parents' (John uttered [with pathos])"
quote10 = "She claims \"George thought it was 'odious' of Miriam to doubt her friend\" "
quote11 = "Ha \"George 'odious'\" blah"
quote12 = "\"'odious'\""

pquote1 = runParserT decoratedInlines "" quote1
pquote2 = runParserT decoratedInlines "" quote2
pquote3 = runParserT decoratedInlines "" quote3
pquote4 = runParserT decoratedInlines "" quote4
pquote5 = runParserT decoratedInlines "" quote5
pquote6 = runParserT decoratedInlines "" quote6
pquote7 = runParserT decoratedInlines "" quote7
pquote8 = runParserT decoratedInlines "" quote8
pquote9 = runParserT decoratedInlines "" quote9
pquote10 = runParserT decoratedInlines "" quote10
pquote11 = runParserT decoratedInlines "" quote11
pquote12 = runParserT decoratedInlines "" quote12


