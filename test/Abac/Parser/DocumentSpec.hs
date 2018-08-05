{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.DocumentSpec where


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

  describe "Document parsers:" $ do

   it "First document" $ do
     pdoc3x >>= (`shouldSatisfy` isRight)

   it "Second document" $ do
     pdoc4x >>= (`shouldSatisfy` isRight)

   it "Third document" $ do
     pdoc5x >>= (`shouldSatisfy` isRight)

   it "Fourth document" $ do
     txt <- readFile "docs/examples/flatland.md"
     let ptext = runParserT doc "" (withoutAbbreviations' txt)
     ptext >>= (`shouldSatisfy` isRight)

   it "Fifth document" $ do
     pdoc6x >>= (`shouldSatisfy` isRight)

   it "Sixth document" $ do
     pflat >>= (`shouldSatisfy` isRight)

   it "Seventh document" $ do
     pmoby >>= (`shouldSatisfy` isRight)

   it "Eighth document" $ do
     ppride >>= (`shouldSatisfy` isRight)

   it "Ninth document" $ do
     palice >>= (`shouldSatisfy` isRight)

   it "Document beginning with newlines" $ do
     pdoc7x >>= (`shouldSatisfy` isRight)

   it "Document with abbreviations" $ do
     pdoc8x >>= (`shouldSatisfy` isRight)






-- Document parsers --
----------------------


doc3x = "---\ntitle: The Prince\nauthor: Nicolo Machiavelli\ntranslator: W. K. Marriott\nnote: a PROJECT GUTENBERG EBOOK\n---\n\n#CHAPTER I -- HOW MANY KINDS OF PRINCIPALITIES THERE ARE, AND BY WHAT\nMEANS THEY ARE ACQUIRED\n\nAll states, all powers, that have held and hold rule over men have been\nand are either republics or principalities.\n\nPrincipalities are either hereditary, in which the family has been long\nestablished; or they are new.\n\nThe new are either entirely new, as was Milan to Francesco Sforza, or\nthey are, as it were, members annexed to the hereditary state of the\nprince who has acquired them, as was the kingdom of Naples to that of\nthe King of Spain.\n\nSuch dominions thus acquired are either accustomed to live under a\nprince, or to live in freedom; and are acquired either by the arms of\nthe prince himself, or of others, or else by fortune or by ability.\n\n\nCHAPTER II -- CONCERNING HEREDITARY PRINCIPALITIES\n\nI will leave out all discussion on republics, inasmuch as in another\nplace I have written of them at length, and will address myself only to\nprincipalities. In doing so I will keep to the order indicated above,\nand discuss how such principalities are to be ruled and preserved.\n\nI say at once there are fewer difficulties in holding hereditary states,\nand those long accustomed to the family of their prince, than new\nones; for it is sufficient only not to transgress the customs of his\nancestors, and to deal prudently with circumstances as they arise, for a\nprince of average powers to maintain himself in his state, unless he\nbe deprived of it by some extraordinary and excessive force; and if he\nshould be so deprived of it, whenever anything sinister happens to the\nusurper, he will regain it.\n\nWe have in Italy, for example, the Duke of Ferrara, who could not have\nwithstood the attacks of the Venetians in '84, nor those of Pope Julius\nin '10, unless he had been long established in his dominions. For the\nhereditary prince has less cause and less necessity to offend; hence it\nhappens that he will be more loved; and unless extraordinary vices cause\nhim to be hated, it is reasonable to expect that his subjects will be\nnaturally well disposed towards him; and in the antiquity and duration\nof his rule the memories and motives that make for change are lost, for\none change always leaves the toothing for another."

doc4x ="---\ntitle: Corporate ipsum\nauthor: Bot\nnote: from a website\n---\n\n# Leverage\n\nLeverage agile frameworks to provide a robust synopsis for high level overviews. Iterative approaches to corporate strategy foster\n collaborative thinking to further the overall value proposition. Organically grow the holistic world view of disruptive innovation via\n workplace diversity and empowerment.\n\n## Win-win\n\nBring to the table win-win survival strategies to ensure proactive domination. At the end of the day, going forward, a new normal\n that\n has evolved from generation X is on the runway heading towards a streamlined cloud solution. User generated content in real-time will have multiple touchpoints for offshoring.\n\n# Capitalize\n\nCapitalize on low hanging fruit to identify a ballpark value added activity to beta test. Override the digital divide with additional clickthroughs from DevOps. Nanotechnology immersion along the information highway will close the loop on focusing solely on the bottom line.\n\n## Media\n\nPodcasting operational change management inside of workflows to establish a framework. Taking seamless key performance indicators offline to maximise the long tail. Keeping your eye on the ball while performing a deep dive on the start-up mentality to derive convergence on cross-platform integration.\n\n### Collaborate\n\nCollaboratively administrate empowered markets via plug-and-play networks. Dynamically procrastinate B2C users after installed base benefits. Dramatically visualize customer directed convergence without revolutionary ROI.\n\n# Surprise efficiently\n\nEfficiently unleash cross-media information without cross-media value. Quickly maximize timely deliverables for real-time schemas. Dramatically maintain clicks-and-mortar solutions without functional solutions.\n\n"


doc5x = "---\ntitle: The Princess\nauthor: Nicolo Malichiavi\ntranslator: John Doe\nnote: a word salad with citations etc\n---\n\n#CHAPTER I -- HOW MANY KINDS OF PRINCIPALITIES THERE ARE, AND BY WHAT\nMEANS THEY ARE ACQUIRED\n\nAll states, all powers, that have held and hold rule over *men* have been\nand are either republics or *principalities* (@malichiavi1800). As remarked by @myfriend2000 (email me similar treasures at me@malikiavi.com), the following seems to be the case: $p \to p$ and $1+1=2$. We have no choice in mathematical matters, $n^2 = n \times n$ .\n\nPrincipalities, as remarked by @anonymous2000, are either hereditary, as in $p \\equiv q$, in which the family @bush1982 has been long\nestablished; or they are *new*.\n\nThe new are either *entirely new*, as was Milan to *Francesco Sforza*, says @malikiavi1880, or\nthey are, as it were, members annexed to the hereditary state (@author1986) of the\nprince who has acquired them, as was the kingdom of Naples to that of\nthe King of Spain.\n\nSuch dominions thus acquired are either accustomed to live under a\nprince, or to live in freedom; and are acquired either by the arms of\nthe prince himself, or of others, or else by fortune or by ability.\n\n\nCHAPTER II -- CONCERNING HEREDITARY PRINCIPALITIES\n\nI will leave out all discussion on republics (for which I refer the dear reader to @mywork1802 and @myfriend1900), inasmuch as in *another\nplace* I have written of them at length (see also @myself1900), and will address myself only to\nprincipalities. In doing so I will keep to the order indicated above,\nand discuss how such principalities are to be ruled and preserved.\n\nI say at once there are fewer difficulties in holding hereditary states,\nand those long accustomed to the family of their prince, than new\nones; for it is *sufficient* only not to transgress the customs of his\nancestors, and to deal prudently with circumstances as they arise, for a\nprince of average **powers** to maintain himself in his state, unless he\nbe deprived of it by some extraordinary and excessive force; and if he\nshould be so deprived of it, whenever anything sinister happens to the\nusurper, he will regain it.\n\nWe have in Italy, for example, the Duke of Ferrara, who could not have\nwithstood the attacks of the Venetians in '84, nor those of Pope Julius\nin '10, @popejulius1699 unless he had been long established in his dominions, as @king1278. For the\nhereditary prince, @prince2001, has less cause and less necessity to offend; hence it\nhappens that he will be more loved; and unless extraordinary vices cause\nhim to be hated, it is reasonable to expect that his subjects will be\nnaturally well disposed towards him; and in the antiquity and duration\nof his **rule the memories and motives** that make for *change* are lost, for\none change always **leaves** the toothing for another. Email the author at malikiavi@phil.org."


pdoc3x = runParserT doc "" doc3x
pdoc4x = runParserT doc "" doc4x
pdoc5x = runParserT doc "" doc5x



pdoc3x_tag = runParserT documentWithTags "" doc3x
pdoc4x_tag = runParserT documentWithTags "" doc4x
pdoc5x_tag = runParserT documentWithTags "" doc5x



pdoc6x :: IO (Either (ParseError Char Void) Document)
pdoc6x = runParserT doc "" doc6x
doc6x :: String
doc6x = "1 Everyone who believes that Jesus is the\n Christ has been born of God, and everyone who loves the parent loves the child. 2 By this we know that we love the children of God, when we love God and obey his commandments. 3 For the love of God is this, that we obey his commandments. And his commandments are not burdensome, 4 for whatever is born of God conquers the world. And this is the victory that conquers the world, our faith. 5 Who is it that conquers\n the world but the one who believes that Jesus is the Son of God? 6 This is the one who came by water and blood, Jesus Christ, not with the water only but with the water and the blood. And the Spirit is the one that testifies, for the Spirit is the truth. 7 There are three that testify: 8 the Spirit and the water and the blood, and these three agree. 9 If we receive human testimony, the testimony of God is greater; for this is the testimony of God that he has testified to his Son. 10 Those who believe in the Son of God have the testimony in their hearts. Those who do not believe in God have made him a liar by not believing in the testimony that God has given concerning his Son. 11 And this is the testimony: God gave us eternal life, and this life is in his Son. 12 Whoever has the Son has life; whoever does not have the Son of God does not have life. 13 I write these things to you who believe in the name of the Son of God, so that you may know that you have eternal life. 14 And this is the boldness we have in him, that if we\n ask anything according to his will, he hears us. 15 And if we know that he hears us in whatever we ask, we know that we have obtained the requests made of him. 16 If you see your brother or sister committing what is not a mortal sin, you will ask, and God will give life to such a one—to those whose sin is not mortal. There is sin that is mortal; I do not say that you should pray about that. 17 All wrongdoing is sin,\n but there is sin that is not mortal. 18 We know that those who are born of God do not sin, but the one who was born of God protects them, and the evil one does not touch them. 19 We know that we are God's children,\n and that the whole world lies under the power of the evil one. 20 And we know that the Son of God has come and has given us understanding so that we may know him who is true; and we are in him who is true, in his Son\n Jesus Christ. He is the true God and eternal life. 21 Little children,\n keep yourselves from idols."


-- bigger texts

pflat :: IO (Either (ParseError Char Void) Document)
pflat = do
  txt <- readFile "docs/examples/flatland.md"
  runParserT doc "" (withoutAbbreviations' txt)

pmoby :: IO (Either (ParseError Char Void) Document)
pmoby = do
  txt <- readFile "docs/examples/mobydick.md"
  runParserT doc "" (withoutAbbreviations' txt)

-- not working because of sentence-level parens, quotes etc.
ppride :: IO (Either (ParseError Char Void) Document)
ppride = do
  txt <- readFile "docs/examples/pride-prejudice.md"
  runParserT doc "" (withoutAbbreviations' txt)

-- not working because of sentence-level parens, quotes etc.
palice :: IO (Either (ParseError Char Void) Document)
palice = do
  txt <- readFile "docs/examples/alice.md"
  runParserT doc "" (withoutAbbreviations' txt)


-- corner cases
pdoc7x = runParserT doc "" $ withoutAbbreviations' "\n\n\n# A section\n\nSome text."
pdoc8x = runParserT doc "" $ withoutAbbreviations' "\n-Some text. (pp. 34-5)\n  + Blah."


