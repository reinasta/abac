{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.POSSpec where

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

import Abac.Parser.InlinesSpec (emph1,emph2)
import Abac.Parser.BlocksSpec (com2,com3)
import Abac.Parser.YamlSpec (yaml3)


spec :: Spec
spec = do

  describe "NLP tags:" $ do
    let inlinesHaveTags ev = isRight ev && length (fromRight [] ev) > 0
    let paraPartHasTags ev = isRight ev && length (taggedParaPart $ fromRight NullPart ev) > 0
    let blockHasTags ev = isRight ev && length (taggedBlock $ fromRight (Para []) ev) > 0
    let blocksHaveTags ev = isRight ev && length (concatMap taggedBlock $ fromRight [] ev) > 0
    let sectionHasTags ev = isRight ev && length (taggedSection $ fromRight (SecBlocks []) ev) > 0
    let sectionsHaveTags ev = isRight ev && length (concatMap taggedSection $ fromRight [] ev) > 0

    it "Styled inlines with tags" $ do
      pemph2_tagged >>= (`shouldSatisfy` inlinesHaveTags)

    it "Styled sentence with tags" $ do
      pemph1_tagged >>= (`shouldSatisfy` paraPartHasTags)

    it "Another styled sentence with tags" $ do
      psntc11_tagged >>= (`shouldSatisfy` paraPartHasTags)

    it "Styled paragraph with tags" $ do
      ppar111_tagged >>= (`shouldSatisfy` blocksHaveTags)

    it "Paragraph with inline comments and tags" $ do
      pcom3_tag >>= (`shouldSatisfy` blockHasTags)

    it "Yaml meta section with tags" $ do
      pyaml3_tag >>= (`shouldSatisfy` sectionsHaveTags)

    it "Many ParaParts with tags" $ do
      pcom2_tag >>= (`shouldSatisfy` isRight)

    it "A full document with tags" $ do
      pdoc1_tag >>= (`shouldSatisfy` isRight)

    it "Another document with tags" $ do
      pdoc2_tag >>= (`shouldSatisfy` isRight)



-- NLP tags --
--------------


par111 = "I think that *this sample sentence was written by the author*, that is, *myself*, this very second. I am proud of it, viz I'd brag about it. But that conviction is no longer accepted. Why is it not accepted? People are said not to know. Some accepted. That is what I think. This is what I'll do. A computation can be made on the spot. Like $5 + 5 = 10$ and \\(p \to p\\). **However, I cannot** but cite @russell1904's book.\n\nExcellent! **Write** me at name@gmail.com" :: T.Text

sntc11 = "A profound formula $p \to p$, and a calculation \\(3 + 4 = 7\\) due to a great thinker's great mind, @Author2030; blah blah."

doc2 = "In the middle of all this, after all that, I was horrified. Sort\nof. The merchandise has been returned in the depot of Berlin. In\nthe depot in Munich there were people who were being punished.\nThat was not acceptable. It was egregious. I was horrified. I\n was flabbergasted. \n\n# Introduction\n\nNot a few of mankind's original thinkers have been colourful\nfigures, led flamboyant lives and thus provided valuable material\nfor many a biography filled with a salacious story or two. Take\nFriedrich Schiller, the German dramatic rebel and accidental\nacademic. When giving his inaugural lecture at the University of\nJena in 1789, he saw that far more people had turned up than could\nbe accommodated in the lecture hall. Rather than let his employers\nfind something bigger nearby, the youthful Schiller seized the\nmoment and marched with the crowd through the streets of Jena\nto the town hall. There he lectured to an enthusiastic crowd of\nthousands shouting ‘freedom’, subsequently enjoying the attention\nhe received from the liberated ladies of the town. Other fighters for\nfreedom and reason, such as Jean-Paul Sartre and Bertrand Russell,\nled eccentric lives that had the local establishments in uproar. Even\na working-class hero like George Orwell could never quite divorce\nhimself from his upper-class public school upbringing, or so his\nbiographers tell us. All such activists – known and unknown –\nfought their battles to improve the lot of ordinary men and women,\nand quite a few advanced science and the arts along the way.\n\nNoam Chomsky is one of the most notable contemporary champions \nof the people. He is also a scientist of the highest calibre. But\nis he great material for a biography? Certainly not, if you ask the\nsubject. An intensely private man, he is horrified to be considered \nthe main character in any story. He jokes about the notion\nthat people come to see him, listen to him, even adore him, when\nin fact he is the most boring speaker ever to hit the stage. He gets\nserious very quickly and tells his audiences that they have come\nto hear about the ‘issues’ of our time, issues that are important to\nthem and, as it happens, to him. What is it that he knows and the\npeople don’t? Wrong question, he would say. The people merely\nwant to know the truth and they know it is hidden from them by a\nvast propaganda machine. His skill is to lift the veil and reveal the\ntruth. Anyone can do it, says Chomsky, it only takes some dedicated\nresearch and logical reasoning.\n\n## Continuation\n\nStop there, Chomsky would say, don’t mix science with political\nactivism. There is no necessary connection between the two, especially\nnot in his case. Like Einstein’s theory of relativity, or Russell’s\nprinciples of mathematical logic, Chomskyan linguistics takes years\nof training and dedication to the scientific method to advance new\ntheories and make new discoveries. Political activism on the other\nhand is the people’s domain, and while scientists are people, too,\nthere is no logical rule that says that a good scientist is also a good\npolitical activist – and quite obviously less so the other way round\n– however much we would like to believe in its possibility.\n\n### Blah blah blah.\n\nSo let’s look at Noam Chomsky as two people: the scientist\n(the linguist) and the political activist. His private life is remark-\nable for its lack of an extraordinary story line. Given the status he\nhas achieved – quite unintended, as we shall see – and the income\nand financial security that come with it, Chomsky is the first to\npoint out that he leads a privileged life, at least in comparison to\nthe working classes in America, and more so when set against the\nabject poverty of the masses of people living in the so-called\nThird World. What is important about Chomsky, however, is tha\nhe is one of those who says what the reasons are for this world of\noppression and blatant social injustice. In return he is vilified by\nthe corporate world of power, including the mainstream press, in\nboth the us and Europe – the German news magazine Der Spiegel\nhas described Chomsky as ‘Ayatollah des antiamerikanischen\nHasses’ (‘the Ayatollah of anti-American hatred’). \n\n# Conclusion\n\nIn 2005 Chomsky celebrated his 77th birthday. He is edging\ncloser to becoming an octogenarian linguist and political activist,\nand bound to remember the public birthday wishes he received\nfrom his wife Carol when he turned 70: ‘Well, seventy is nice, but\nwhat I’m really looking forward to is eighty!’ 3 On his birthday, as\non every other day of the year, he receives some 200 e-mails deal-\ning with linguistics, politics and other matters. He answers them\nall, every day of the week (though, befitting his many responsibili-\nties, a couple of personal assistants help him in the process). In\naddition he prepares speeches, lecture notes, learned articles, his\nlatest book and other writing tasks. As a retired Emeritus Institute\nProfessor of Linguistics at the Massachusetts Institute of Tech-\nnology (mit) he still contributes to teaching and research well\nbeyond the call of duty. Chomsky is a ferocious reader and reads\nwith great attention to detail. Over the years he has acquired an\nencyclopaedic knowledge. As such he never stops working. His\noutput and achievements are enormous, yet he would say that a\nhumble factory worker on the assembly line produces far more\nthan he has ever done. He is deeply aware that his status within\nthe working classes is a privileged one. Still, the true nature of an\nacademic worker is embodied in Chomsky.\nPrepositions: in on of at from of of of.\nPassives: was called, were named, have been argued, is sent.\n"

-- nlp tags
tag1 = "/NNS "
tag2 = "/JJ "
tag3 = "/. "

--Remove tags from math strings and citations

nlpString1 = "$5/NNS +/NN 5/CD =/NN 10$/CD"
nlpString2 = "\\(/JJ p/NN o/NN p/NN \\)"
nlpString3 = "@/JJ russell19004/NN"

pNlpString3 = runParserT citationWithTag "" nlpString3

--the correct results should be equal up to set-membership of the non-space characters

correctNlp1 = "$5 + 5 = 10$"
correctNlp2 = "\\(p \to p\\)"


ptag1 = runParserT nlpTag "" tag1
ptag2 = runParserT nlpTag "" tag2
ptag3 = runParserT nlpTag "" tag3

pNlpString1 = runParserT mathInline "" nlpString1



pemph1_tagged = runParserT paraPartWithTags "" emph1
pemph2_tagged = runParserT (styledInlines >>= tagInlines) "" emph2

psntc11_tagged = runParserT paraPartWithTags "" $ T.unpack sntc11
ppar111_tagged = runParserT (many blockWithTags) "" $ T.unpack par111

pcom2_tag = runParserT (many paraPartWithTags) "" com2
pcom3_tag = runParserT blockWithTags "" com3


pyaml3_tag = runParserT sectionsWithTags "" yaml3

pdoc1_tag = runParserT documentWithTags "" $ yaml3 ++ nls ++ com2 ++ nls ++ com3 where nls = "\n\n"
pdoc2_tag = runParserT documentWithTags "" doc2

-- NLP tags
nlptag1 = "In the middle of all this hullabaloo, after all that struggle, I was horrified. Sort\nof. The merchandise has been returned in the depot of Berlin untouched, which is abominable. It was badly used. In\nthe depot in Munich things were even worse: there were people who were being punished for eating the only remaining food supplies.\nThat was unacceptable. It was egregious. I was really horrified. I\n was flabbergasted. "

pnlptag1 = runParserT blockWithTags "" nlptag1

