{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.ParaPartsSpec where

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
import Abac.Parser.InternalSpec


spec :: Spec
spec = do

  describe "Sentence parser:" $ do
    let dummy = Sentence []
    let isSentence' v = isRight v && (isSentence . fromRight dummy) v
    let isParaPart' v = isRight v && (isParaPart . fromRight dummy) v

    it "This is a sentence" $ do
      parseRes <- fmap isSentence <$> psntc1
      parseRes `shouldBe` Right True

    it "A couple of sentences" $ do
      parseRes <- fmap (all isSentence) <$> psntc2
      parseRes `shouldBe` Right True

    it "More sentences" $ do
      parseRes <- fmap (all isSentence) <$> psntc3
      parseRes `shouldBe` Right True

    it "Sentences that begin with spaces" $ do
      parseRes <- fmap (all isSentence) <$> psntc4
      parseRes `shouldBe` Right True

    it "Sentences that begin with spaces, again" $ do
      parseRes <- fmap (all isSentence) <$> psntc5
      parseRes `shouldBe` Right True

    it "This is a sentence with a linked image" $ do
      parseRes <- fmap isSentence <$> psntc6
      parseRes `shouldBe` Right True

    it "A sentence followed by marker is (still) a sentence" $ do
     psntc7 >>= (`shouldSatisfy` isSentence')

    it "A sentence followed by a non-sentence is (still) a sentence" $ do
     psntc8 >>= (`shouldSatisfy` isSentence')

    it "A couple of inlines followed by multiple newlines" $ do
     psntc9 >>= (`shouldSatisfy` isParaPart')


    -- various paragraph parts, e.g. Inlines
  describe "Small footnote parsers:" $ do

    it "A sentence followed by a sentence-level footnote; return the parsed footnote" $ do
      pfootnote1 >>= (`shouldSatisfy` isRight)

    it "A sentence followed by a sentence-level footnote; both get parsed" $ do
      pfootnote2 >>= (`shouldSatisfy` isRight)

    it "A sentence containing a word-level, footnote which is styled and bracketed" $ do
      pfootnote3 >>= (`shouldSatisfy` isRight)

    it "A sentence followed by a sentence-level footnote which is styled and bracketed" $ do
      pfootnote4 >>= (`shouldSatisfy` isRight)

    it "A sentence with various styled small footnotes" $ do
      pfootnote5 >>= (`shouldSatisfy` isRight)




    -- various paragraph parts, e.g. Inlines
  describe "Other paragraph parts:" $ do

    it "An Inlines-value with just an empty quotation is skipped by the para-parts parser" $ do
      pprt1 >>= (`shouldSatisfy` (== Right []))

    it "A sentence within brackets" $ do
      psentbrack1 >>= (`shouldSatisfy` isRight)

    it "Two sentences within brackets" $ do
      psentbrack2 >>= (`shouldSatisfy` isRight)

    it "Three sentences (one of which is itself bracketed) within brackets" $ do
      psentbrack3 >>= (`shouldSatisfy` isRight)

    it "Multi-bracketed sentences" $ do
      psentbrack4 >>= (`shouldSatisfy` isRight)

    it "Bracketed and unbracketed sentences" $ do
      psentbrack5 >>= (`shouldSatisfy` isRight)

    it "Bracketed and double-quoted sentences" $ do
      psentbrack6 >>= (`shouldSatisfy` isRight)

    it "Sentences with parentheses, brackets and double-quotes" $ do
      psentbrack7 >>= (`shouldSatisfy` isRight)

    it "Sentences with or without brackets and double-quotes" $ do
      psentbrack8 >>= (`shouldSatisfy` isRight)

    it "Sentences with curly double-quotes and emphasis" $ do
      psentbrack9 >>= (`shouldSatisfy` isRight)

    it "A sentence with curly double-quotes" $ do
      psentbrack10 >>= (`shouldSatisfy` isRight)

    it "A sentence with *-marked italics after a newline (not to be confused with a bullet point)" $ do
      psentbrack11 >>= (`shouldSatisfy` isRight)


    -- sentences with balanced punctuation
    it "A sentence with seven single-quoted words, three bracketed ones, five parenthetical ones, and a bold one" $ do
      let
        sentHas n attrs einls =
          let Right (Sentence inls) = einls
              words = filter wordlike inls
              targets = traverseWith (`hasAttrs` attrs) (Inlines words)
          in  length targets == n

        --sent1Prop :: Either (ParseError Char Void) [Inline] -> Bool;
        sent1Prop einls =
            sentHas 7 [Quoted] einls
            && sentHas 3 [Bracketed] einls
            && sentHas 5 [Parenthetical] einls
            && sentHas 1 [Bold] einls
      psentblnc1 >>= (`shouldSatisfy` sent1Prop)

    it "A sentence with fourteen double-quoted words, an italic, a bold one, three bracketed ones, and four parentheticals" $ do
      let --sent2Prop :: Either (ParseError Char Void) [Inline] -> Bool;
          sent2Prop einls =
            sentHas 14 [DoubleQuoted] einls
            && sentHas 1 [Emph] einls
            && sentHas 1 [Bold] einls
            && sentHas 3 [Bracketed] einls
            && sentHas 5 [Parenthetical] einls
      psentblnc2 >>= (`shouldSatisfy` sent2Prop)

    it "A sentence with six italicized words, a bold-and-italic one, and five bracketed words" $ do
      let --sent3Prop :: Either (ParseError Char Void) [Inline] -> Bool;
          sent3Prop einls =
            sentHas 6 [Emph] einls
            && sentHas 1 [Emph,Bold] einls
            && sentHas 5 [Bracketed] einls
      psentblnc3 >>= (`shouldSatisfy` sent3Prop)



-- Sentence parser --
---------------------

sntc1 = "A sentence\nwith new lines\nthat ends with\n a question mark?\n"
sntc2 = "A sentence\nwith new lines\nthat ends with\n a question mark?\nShort sentence.\n"
sntc3 = "A sentence\nwith new lines\nthat ends with\n a question mark?\nShort sentence.\n\n"
sntc4 = "  A sentence\nwith new lines\nthat ends with\n a question mark?\n  Short sentence.\n\n"
sntc5 = "   Pabst: af fashion axe fam biodiesel, heirloom tote bag.\n Bespoke lumbersexual ethical, pinterest leggings vexillologist hammock coloring book.\n Four loko poutine semiotics selfies next level."
sntc6 = "Here is an image: ![Alt text][id]."
sntc7 = "Blah.\n   b. Yuccie health goth"
sntc8 = "Blah.\nBlah blah"
sntc9 = "He is edging *closer to becoming an octogenarian linguist* and *political activist,* and bound to remember the public birthday wishes he received from his wife Carol when he turned 70: ‘Well, seventy is nice, but what I’m really looking forward to is eighty!’\n\n\n"
sntc10 = "A profound formula $p \to p$, and a calculation \\(3 + 4 = 7\\) due to a great thinker's great mind, @IKant2030; blah blah."


psntc1 = runParserT sentence "" sntc1
psntc2 = runParserT (many sentence) "" sntc2
psntc3 = runParserT (many sentence) "" sntc3 -- must fail
psntc4 = runParserT (many sentence) "" sntc4
psntc5 = runParserT (many sentence) "" sntc5
psntc6 = runParserT sentence "" sntc6
psntc7 = runParserT sentence "" sntc7
psntc8 = runParserT sentence "" sntc8
psntc9 = runParserT someParaPart "" sntc9
psntc10 = runParserT sentence "" sntc10

-- Other paragraph parts --
---------------------------

pprt1 = runParserT decoratedParaParts "" ("\"\"\n\n" :: String)

-- balancing punctuation

sentbrack1 :: String
sentbrack1 = "[A sentence within brackets.]"

sentbrack2 :: String
sentbrack2 = "[A sentence within brackets. And another one.]"

sentbrack3 :: String
sentbrack3 = "[A sentence within brackets. [Another sentence withing two pairs of brackets.] That's it! ]"

sentbrack4 :: String
sentbrack4 = "[A sentence within brackets. [Another sentence withing two pairs of brackets.] We are not done! [Here is another one!] ]"

sentbrack5 :: String
sentbrack5 = "A sentence without brackets. [Two sentences with brackets. They end here!]"

sentbrack6 :: String
sentbrack6 = "\"A double-quoted sentence [with inner brackets].\" [A bracketed sentence, with \"inner quotes\". Another (bracketed) sentence with parens. And some inlines ]"

-- this is not parsed quite correctly, as "[And Chrim said]" is parsed as a separate Inlines value
-- rather than as part of the sentence as what follows it.
sentbrack7 :: String
sentbrack7 = "(I asked her \"Do you mind?\" . [And Chris said] \"Okay, but only once!\"; and we all enjoyed the evening.)"

-- not quite right: the "be brave" injunction is treated as a sentence and "He said" as an Inlines value
sentbrack8 :: String
sentbrack8 = "I'm telling you what he [John] said. He said \"be brave.\" [But no-one listened.] "

sentbrack9 :: String
sentbrack9 = "“Design! Nonsense, how can you talk so! But it is very likely that he\n*may* fall in love with one of them, and therefore you must visit him as\nsoon as he comes.”"

sentbrack10 :: String
sentbrack10 = "“Is that his design in settling here?”"

sentbrack11 :: String
sentbrack11 = "he\n*may* fall in love with one"

-- parsing

psentbrack1 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack1 = runParserT decoratedParaParts "" sentbrack1

psentbrack2 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack2 = runParserT decoratedParaParts "" sentbrack2

psentbrack3 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack3 = runParserT decoratedParaParts "" sentbrack3

psentbrack4 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack4 = runParserT decoratedParaParts "" sentbrack4

psentbrack5 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack5 = runParserT decoratedParaParts "" sentbrack5

psentbrack6 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack6 = runParserT decoratedParaParts "" sentbrack6

psentbrack7 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack7 = runParserT decoratedParaParts "" sentbrack7

psentbrack8 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack8 = runParserT decoratedParaParts "" sentbrack8

psentbrack9 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack9 = runParserT decoratedParaParts "" sentbrack9

psentbrack10 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack10 = runParserT decoratedParaParts "" sentbrack10

psentbrack11 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentbrack11 = runParserT decoratedParaParts "" sentbrack11


pfootnote1 :: IO (Either (ParseError (Token String) Void) [ParaPart])
pfootnote1 = runParserT (someParaPart *> some footnoteP) "" ftn1

pfootnote2 :: IO (Either (ParseError (Token String) Void) [ParaPart])
pfootnote2 = runParserT (some paraPart) "" ftn2

pfootnote3 :: IO (Either (ParseError (Token String) Void) [ParaPart])
pfootnote3 = runParserT (some paraPart) "" ftn3

pfootnote4 :: IO (Either (ParseError (Token String) Void) [ParaPart])
pfootnote4 = runParserT (some paraPart) "" ftn4

pfootnote5 :: IO (Either (ParseError (Token String) Void) [ParaPart])
pfootnote5 = runParserT (some paraPart) "" ftn5


ftn1 = "I call our world Flatland, not because we call it so, but to make its\nnature clearer to you, my happy readers, who are privileged to live in\nSpace.^[This is an excerpt from Flatland by Edwin Abbot.]\n"

ftn2 = "I call our world Flatland, not because we call it so, but to make its\nnature clearer to you, my happy readers, who are privileged to live in\nSpace.^[This is an excerpt from Flatland by Edwin Abbot]\n"

ftn3 = "I call our world^[A *nice world* [that is].] Flatland."

ftn4 = "I call our world Flatland, not because we call it so, but to make its\nnature clearer to you, my happy readers, who are privileged to live in\nSpace.^[This is an excerpt from 'Flatland' (by Edwin Abbot) [author's note]]\n"

ftn5 = "I call our world^[A *nice world* [that is].] Flatland, not because we call it so, but to make its\nnature clearer to you, my happy readers, who are privileged to live in\nSpace.^[This is an excerpt from 'Flatland' (by Edwin Abbot)]\n"


sentquote1, sentquote2 :: String
sentquote1 = "“But it is,” returned she; “for mrs Long has just been here, and she told me all about it.”\n\n"
sentquote2 = "“How doth the little--”"

psentquote1, psentquote2 :: IO (Either (ParseError (Token String) Void) [ParaPart])
psentquote1 = runParserT decoratedParaParts "" sentquote1
psentquote2 = runParserT decoratedParaParts "" sentquote2

-- sentences with parens, quotation marks and other matching punctuation
sentblnc1 = "'Of course [cough] I love my parents' [John uttered], (and he really **meant** it)!"
sentblnc2 = "She *claims* \"George thought it was 'odious' [his word] of Miriam to **doubt** her friend (Paul)\" (but who cares [nobody])."
sentblnc3 = "I'll show you *some italics and a **bold** one* [nothing out of the ordinary]. "

psentblnc1 = runParserT sentence "" sentblnc1
psentblnc2 = runParserT sentence "" sentblnc2
psentblnc3 = runParserT sentence "" sentblnc3

