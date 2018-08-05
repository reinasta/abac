{-# LANGUAGE OverloadedStrings #-}
module Abac.Parser.BlocksSpec where

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

-- this module contains tests for Comments parsers, including block-level
-- comment parsers but also inline comment parsers.

spec :: Spec
spec = do

  describe "Paragraph parser:" $ do

    it "This is a paragraph" $ do
      (fmap . fmap) isParagraph par1Parser >>= (`shouldBe` Right True)

    it "This is a paragraph with newlines" $ do
      (fmap . fmap) isParagraph par2Parser >>= (`shouldBe` Right True)

    it "This is a paragraph with short footnotes" $ do
      (fmap . fmap) isParagraph par3Parser >>= (`shouldBe` Right True)

    it "This is a paragraph with citations" $ do
      (fmap . fmap) isParagraph prp1 >>= (`shouldBe` Right True)

    it "This is a paragraph with inline math and citations" $ do
      (fmap . fmap) isParagraph prp2 >>= (`shouldBe` Right True)

    it "This is a paragraph with inline math" $ do
      (fmap . fmap) isParagraph prp3 >>= (`shouldBe` Right True)

    it "Another paragraph with inline math" $ do
      (fmap . fmap) isParagraph prp4  >>= (`shouldBe` Right True)

    it "Another paragraph with short footnotes, inline math, and citations" $ do
      (fmap . fmap) isParagraph prp5 >>= (`shouldBe` Right True)

    it "A paragraph that ends with a linked image" $ do
      (fmap . fmap) isParagraph prp6 >>= (`shouldBe` Right True)

    it "Another paragraph that ends with a linked image" $ do
      (fmap . fmap) (any isParagraph) prp7 >>= (`shouldBe` Right True)


  describe "Block parser:" $ do

    it "Parse a block (paragraph) that ends with a linked image" $ do
     (fmap . fmap) isBlock pblc1 >>= (`shouldBe` Right True)

    it "Parse two blocks with linked images and reference images" $ do
     (fmap . fmap) (any isBlock) pblc2 >>= (`shouldBe` Right True)

    it "Parse four blocks: a paragraph and three example blocks" $ do
     (fmap . fmap) (length . filter isBlock) pblc3 >>= (`shouldBe` Right (4 :: Int))

    it "Parse five blocks consisting of bullet items and examples" $ do
     (fmap . fmap) (length . filter isBlock) pblc4 >>= (`shouldBe` Right (5 :: Int))

    it "A block quotation containing five other blocks" $ do
     (fmap . fmap) (length . filter isBlock) pblc5 >>= (`shouldBe` Right (1 :: Int))

    it "Two block quotations" $ do
     (fmap . fmap) (length . filter isBlock) pblc5' >>= (`shouldBe` Right (2 :: Int))

    it "Quotations cannot cross block boundaries" $ do
     (fmap . fmap) (length . filter isBlock) pblc51' >>= (`shouldBe` Right (0 :: Int))

    it "A block" $ do
     (fmap isBlock <$> pblc54) >>= (`shouldBe` Right True)

    it "Another block with many quotation marks" $ do
     (fmap isBlock <$> pblc55) >>= (`shouldBe` Right True)

    it "A paragraph with an empty quotation" $ do
     (fmap isBlock <$> pblc6) >>= (`shouldBe` Right True)

    it "A block quote with an empty quotation" $ do
     (fmap isBlock <$> pblc61) >>= (`shouldBe` Right True)

    it "An empty block quote" $ do
     (fmap isBlock <$> pblc611) >>= (`shouldBe` Right True)

    it "A simple block" $ do
     (fmap isBlock <$> pblc7) >>= (`shouldBe` Right True)

    it "A block with many newlines" $ do
     (fmap isBlock <$> pblc8) >>= (`shouldBe` Right True)


  describe "Footnote parsers:" $ do

    it "A short (in-paragraph) footnote." $ do
      (fmap isFootnoteS <$> pftn1) >>= (`shouldBe` Right True)

    it "Another short (in-paragraph) footnote." $ do
      (fmap isFootnoteS <$> pftn5) >>= (`shouldBe` Right True)

    it "A big (block) footnote." $ do
      (fmap isFootnoteB <$> pftn2) >>= (`shouldBe` Right True)

    it "A big (block) footnote with embedded footnotes and examples" $ do
      (fmap isFootnoteB <$> pftn3) >>= (`shouldBe` Right True)

    it "A block with footnotes sprinkled on both words and sentences" $ do
      (fmap isBlock <$> pftn4) >>= (`shouldBe` Right True)

    it "A mulit-paragraph (big) footnote" $ do
      (fmap isBlock <$> pftn6) >>= (`shouldBe` Right True)


  describe "Comment parsers:" $ do

    it "This is a sentence-level comment" $ do
     parseRes <- (fmap . fmap) isParaComment pcom0
     parseRes `shouldBe` Right True

    it "This sentence has two inline comments" $ do
     parseRes <- (fmap . fmap) (length . filter isInlineComment . unwrapParaPart') pcom1
     parseRes `shouldBe` Right (2 :: Int)

    it "Two sentence-level comments inside a block" $ do
     parseRes <- (fmap . fmap) (length . filter isParaComment . blockToParaParts) pcom2
     parseRes `shouldBe` Right (2 :: Int)

    it "This is a block-level comment" $ do
     parseRes <- (fmap . fmap) isBlockComment pcom3
     parseRes `shouldBe` Right True

    it "This is another a block-level comment" $ do
     parseRes <- (fmap . fmap) isBlockComment pcom4
     parseRes `shouldBe` Right True

    it "This is an inline comment (e.g. inside a sentence)" $ do
     parseRes <- (fmap . fmap) (any isInlineComment . unwrapParaPart') pcom5
     parseRes `shouldBe` Right True

    it "This is another sentence-level comment (inside a block)" $ do
      parseRes <- (fmap . fmap) (any isParaComment . blockToParaParts) pcom6
      parseRes `shouldBe` Right True



-- Paragraphs --
----------------

-- Problems: paragraphs starting with spaces, footnotes occuring after newline

par1Parser = runParserT paragraph "" par1
par1 = "Pabst af fashion axe fam biodiesel,\n heirloom tote bag.\n Bespoke lumbersexual ethical, pinterest leggings vexillologist hammock coloring book.\n Four loko poutine semiotics selfies next level.\n Helvetica listicle seitan, man bun\n austin VHS 90's iPhone semiotics celiac neutra tattooed master cleanse meggings. Kale chips mumblecore enamel pin tote bag try-hard tumblr microdosing readymade keytar 90's before they sold out portland subway tile waistcoat. Vape you probably haven't heard of them mustache, locavore vegan godard seitan raw denim quinoa palo santo lo-fi jianbing.\n Actually fam williamsburg, taiyaki jean shorts shabby chic tofu poke tattooed chicharrones plaid beard.\n Seitan\n chambray flannel gochujang taiyaki pitchfork, pug asymmetrical keffiyeh health goth. Quinoa forage edison\n bulb flexitarian meggings authentic lumbersexual man bun.\n Cloud bread twee mumblecore subway tile plaid. Copper mug sustainable hoodie chia venmo. Art party slow-carb synth, banjo pickled raclette small batch lo-fi aesthetic tumeric fashion axe trust fund keffiyeh cliche.\n\n\n"

par2Parser = runParserT paragraph "" par2
par2 = "  Pabst: af fashion axe fam biodiesel, heirloom tote bag.\n Bespoke lumbersexual ethical, pinterest leggings vexillologist hammock coloring book.\n Four loko poutine semiotics selfies next level. Helvetica listicle seitan, man bun austin VHS 90's iPhone semiotics celiac neutra tattooed master cleanse meggings. Kale chips mumblecore enamel pin tote bag try-hard tumblr microdosing readymade keytar 90's before they sold out portland subway tile waistcoat.\n Vape you probably haven't heard of them mustache, locavore vegan godard seitan raw denim quinoa palo santo lo-fi jianbing. Actually fam williamsburg, taiyaki jean shorts shabby chic tofu poke tattooed chicharrones plaid beard. Seitan chambray flannel gochujang taiyaki pitchfork, pug asymmetrical keffiyeh health goth.\n Quinoa forage edison bulb flexitarian meggings authentic lumbersexual man bun. Cloud bread twee mumblecore subway tile plaid. Copper mug sustainable hoodie chia venmo.\n Art party slow-carb synth, banjo pickled raclette small batch lo-fi aesthetic tumeric fashion axe trust fund keffiyeh cliche.\n\n"

par3Parser = runParserT paragraph "" par3
par3 = "Polaroid gochujang whatever, deep v four dollar toast kitsch authentic +1 banjo poke synth single-origin coffee DIY\n gluten-free.\n^[This is a footnote. It consists of two sentences!] Paleo ugh\n retro, letterpress skateboard dreamcatcher blog. Truffaut biodiesel sartorial activated charcoal, cred direct trade cray iPhone. Chia leggings narwhal green juice aesthetic migas fashion axe coloring book distillery keytar fingerstache ennui heirloom gochujang. Listicle try-hard synth master cleanse letterpress seitan before they sold out fanny pack ramps kombucha put a bird on it.^[Another short footnote. A stupid one, to be sure.] Echo park readymade four loko mustache.\n Everyday carry tumeric helvetica kickstarter tattooed palo santo subway tile, pitchfork aesthetic health goth. Synth schlitz sartorial fanny pack authentic, williamsburg wolf kombucha truffaut. Taiyaki bushwick vegan lo-fi tbh flannel. Shabby chic farm-to-table +1, keytar lyft knausgaard iPhone pork belly health goth tilde edison bulb pickled. Tote bag schlitz asymmetrical, succulents crucifix leggings live-edge. Single-origin coffee farm-to-table tattooed 90's, brunch kinfolk pinterest freegan deep v mumblecore green juice retro. Brooklyn kinfolk venmo cloud bread, helvetica raw denim cliche 3 wolf moon microdosing brunch stumptown gentrify messenger bag lomo. Fam cliche listicle wolf, quinoa poke venmo YOLO.^[Lastly,\n a short footnote.]\n\n"


pr1 = "This is a sentence. This is also one. What about a question at this point? We continue with a longer sentence that cites @author2003, @plato, and @aristotle, and computes $1 + 3 = 4$. No so hard, heh? \n\n"

pr2 = "We begin with a a short sentence, to the point. We follow with an even shorter one. What about a question? This shows that we are not afraid to ask and that we are used to being in control of our environment. We continue with a longer sentence that cites @author2003, @plato, and @aristotle, and computes $1 + 3$ and $log_2 1000$ to show off a bit. No so hard, heh?\n\n"

pr3 = "A sentence. Another one $p \to q \& r$, and $q$.\n\n"

pr4 = "A sentence. Another one with $p \to q \& r$ and $q$.\n\n"

pr5 = "This is a sentence.^[It contains a multiple-sentence footnote. This, for instance, is the second sentence in the footnote.] This is also a sentence. What about a question at this point? We continue with a longer sentence that cites @author2003, @plato, and @aristotle, and computes $1 + 3 = 4$. No so hard, heh?^[Note that a in-paragraph footnotes cannot contain examples, lists or other blocks.] \n\n"

pr6 = "Here is an image: ![Alt text][id].\n\n"

pr7 = "Here is an image: ![Alt text][id].\n\n " -- note the space at the end of the string

prp1 = runParserT paragraph "" pr1
prp2 = runParserT paragraph "" pr2
prp3 = runParserT paragraph "" pr3
prp4 = runParserT paragraph "" pr4
prp5 = runParserT paragraph "" pr5
prp6 = runParserT paragraph "" pr6
prp7 = runParserT (many paragraph) "" pr7

-- Blocks --
------------


blc1 = " [id]: url/to/image  \"Optional title attribute\""
blc2 = "Here is an image: ![Alt text][id].\n\n [id]: url/to/image  \"Optional title attribute\""

-- `inlines` is broken, as it succeeds on "A\nsubordinated\nitem.", parsing the first three items
blc3 = "I think that she thinks that I think that we think that we believe that it is not nice that they think of us in that way. Which way? The way they think we think that you think at this very moment.\n\n(@1) Quick sentence.^[This is\n a footnote.\nIt spans\n two sentences.]\nThis sentence.\n^[Finally a footnote!]\n  - A\nsubordinated\nitem.\n  - Subordinated\nitem number\ntwo.\n(@2) Blah.\n  + a list of items: car, disc, banana\n  + another list: book, computer, blah;\n  c. First sentence! Second sentence. Third sentence, at last!\n  + blah,\nblah\nblah.\n\n(@3) Another hanging example.\n\n  a. With a short sub-example. Really short!\n\n"

blc3' = "I think.\n\n(@1) Quick sentence.^[This is\n a footnote.\nJust that.]"

blc4 = "\n(@1) Quick sentence.^[This is\n a footnote.\nIt spans\n two sentences.]\nThis sentence.\n^[Finally a footnote!]\n  - A\nsubordinated\nitem.\n  - Subordinated\nitem number\ntwo.\n\n\n+ I think that:\n  - she thinks that\n  - I think that\n  - we think that we believe that it is not nice that they think of us in that way.\n+ Which way?\n  - The way they think we think that you think at this very moment.\n\n(@2) Blah.\n  + a list of items: car, disc, banana\n  + another list: book, computer, blah;\n  c. First sentence! Second sentence. Third sentence, at last!\n  + blah,\nblah\nblah.\n\n(@3) Another hanging example.\n\n  a. With a short sub-example. Really short!\n\n"

-- note the way we write examples/items in blockquotes: ">\n (@1) Blah" or ">\n + Blah"
blc5 = "> Polaroid gochujang whatever, deep v four dollar toast kitsch authentic +1 banjo poke synth single-origin coffee DIY gluten-free. Ergo:\n\n> \n- Paleo ugh retro, letterpress skateboard dreamcatcher blog. Truffaut biodiesel sartorial activated charcoal, cred direct trade cray iPhone.\n - Chia leggings narwhal green juice aesthetic migas fashion axe coloring book distillery keytar fingerstache ennui heirloom gochujang.\n\n> > Short embedded (block quoted) paragraph.\n\n\n> > > And people say things, such as:\n\n> > > > \n(@) Listicle try-hard synth master cleanse letterpress seitan before they sold out fanny pack ramps kombucha put a bird on it.\n  a. Echo park readymade four loko mustache. Everyday carry tumeric helvetica kickstarter tattooed palo santo subway tile, pitchfork aesthetic health goth. Synth schlitz sartorial fanny pack authentic, williamsburg wolf kombucha truffaut.\n  b. Taiyaki bushwick vegan lo-fi tbh flannel. Shabby chic farm-to-table +1, keytar lyft knausgaard iPhone pork belly health goth tilde edison bulb pickled. Tote bag schlitz asymmetrical, succulents crucifix leggings live-edge.\n\n> Single-origin coffee farm-to-table tattooed 90's, brunch kinfolk pinterest freegan deep v mumblecore green juice retro. Brooklyn kinfolk venmo cloud bread, helvetica raw denim cliche 3 wolf moon microdosing brunch stumptown gentrify messenger bag lomo. Fam cliche listicle wolf, quinoa poke venmo YOLO.\n\n"

blc5' = "> Polaroid gochujang whatever, deep v four dollar toast kitsch authentic +1 banjo poke synth single-origin coffee DIY gluten-free.\n\n> > Ergo: Paleo ugh retro, letterpress skateboard dreamcatcher blog.\n\n> Truffaut biodiesel sartorial activated charcoal, cred direct trade cray iPhone.\n"

-- quotation marks cannot cross (sub)block boundaries (within the BlockQuotes constructor)
blc51' = "> \"Polaroid gochujang whatever, deep v four dollar toast kitsch authentic +1 banjo poke synth single-origin coffee DIY gluten-free.\n\n> > Ergo: Paleo ugh retro, letterpress skateboard dreamcatcher blog.\n\n> Truffaut biodiesel sartorial activated charcoal, cred direct trade cray iPhone.\"\n"

--test
blc54 :: String
blc54 = "Windows there are none in our houses: for the light comes to us alike\nin our homes and out of them, by day and by night, equally at all times\nand in all places, whence we know not. It was in old days, with our\nlearned men, an interesting and oft-investigated question, \"What is the\norigin of light?\" and the solution of it has been repeatedly attempted,\nwith no other result than to crowd our lunatic asylums with the\nwould-be solvers. Hence, after fruitless attempts to suppress such\ninvestigations indirectly by making them liable to a heavy tax, the\nLegislature, in comparatively recent times, absolutely prohibited them.\nI--alas, I alone in Flatland--know now only too well the true solution\nof this mysterious problem; but my knowledge cannot be made\nintelligible to a single one of my countrymen; and I am mocked at--I,\nthe sole possessor of the truths of Space and of the theory of the\nintroduction of Light from the world of three Dimensions--as if I were\nthe maddest of the mad! But a truce to these painful digressions: let\nme return to our houses."

blc55 :: String
blc55 = "‘I’m sure I’m not Ada,’ she said, ‘for her hair goes in such long\nringlets, and mine doesn’t go in ringlets at all; and I’m sure I can’t\nbe Mabel, for I know all sorts of things, and she, oh! she knows such a\nvery little! Besides, SHE’S she, and I’m I, and--oh dear, how puzzling\nit all is! I’ll try if I know all the things I used to know. Let me\nsee: four times five is twelve, and four times six is thirteen, and\nfour times seven is--oh dear! I shall never get to twenty at that rate!\nHowever, the Multiplication Table doesn’t signify: let’s try Geography.\nLondon is the capital of Paris, and Paris is the capital of Rome, and\nRome--no, THAT’S all wrong, I’m certain! I must have been changed for\nMabel! I’ll try and say “How doth the little--”’ and she crossed her\nhands on her lap as if she were saying lessons, and began to repeat it,\nbut her voice sounded hoarse and strange, and the words did not come the\nsame as they used to do:"



blc6 = "\"\"\n\n"
blc61 = "> \"\"\n\n"
blc611 = "> \n\n"
blc7 = "This is a paragraph in a certain section (number 1 3 1 1).\n\n"
blc8 = "This\n is\n a\n paragraph in a certain section\n (number 1 3 1 1).\n\n\n"



pblc1 = runParserT block "" blc1
pblc2 = runParserT (many block) "" blc2
pblc3 = runParserT (many block) "" blc3
pblc3' = runParserT (many block) "" blc3'
pblc4 = runParserT (many block) "" blc4
pblc5 = runParserT (many block) "" blc5
pblc5' = runParserT (many block) "" blc5'
pblc51' = runParserT (many block) "" blc51' -- should fail

pblc54 :: IO (Either (ParseError Char Void) Block)
pblc54 = runParserT block "" blc54

pblc55 :: IO (Either (ParseError Char Void) Block)
pblc55 = runParserT block "" blc55


pblc6 = runParserT block "" blc6
pblc61 = runParserT block "" blc61
pblc611 = runParserT block "" blc611
pblc7 = runParserT block "" blc7
pblc8 = runParserT block "" blc8


-- test for newlinePlusMarker: three newlines plus marker (i.e. newline followed by e.g. " (@1) ")
newlinePlusMarker_txt1 = "Blah.\n\n\n  (@ex1) Just this.\n\n"



-- Footnote parsers --
----------------------

ftn1 = "^[It contains a multiple-sentence footnote. This, for instance, is the second sentence in the footnote.]"
ftn2 = "[^ftn1]: Food truck jean shorts organic activated charcoal quinoa. Yuccie health goth swag, man bun PBR&B farm-to-table tumeric four loko pok pok sriracha. Squid locavore tumeric man bun, mumblecore blue bottle schlitz. Snackwave mlkshk 3 wolf moon chillwave bicycle rights gastropub migas gentrify vice cold-pressed church-key scenester art party squid jianbing. Sustainable chicharrones pour-over, ugh 90's austin freegan kitsch blue bottle taxidermy wayfarers poutine photo booth leggings migas. Heirloom single-origin coffee +1 waistcoat vinyl hoodie green juice enamel pin distillery etsy. Man bun aesthetic godard echo park gentrify. Vape hot chicken flannel, post-ironic kale chips messenger bag street art thundercats dreamcatcher seitan mixtape mustache shoreditch roof party. Kitsch irony occupy, tofu kogi mustache hashtag migas. Vape butcher 3 wolf moon beard knausgaard wolf trust fund kinfolk ethical slow-carb. Ugh ethical tacos letterpress, seitan ennui hoodie salvia. Tumeric pabst bushwick vaporware pinterest.\n\n"

ftn3 = "[^ftn45]: Kitsch irony occupy, tofu kogi mustache hashtag migas.^[A short footnote inside a big footnote.] Vape butcher 3 wolf moon beard knausgaard wolf trust fund kinfolk ethical slow-carb. An example:\n\n  (1) The first example.\n  a. Sub-example blah 1.\n  b. Sub-example blah 2.\n  (@2) Another main example.\nUgh ethical tacos letterpress, seitan ennui hoodie salvia. Tumeric pabst bushwick vaporware pinterest.\n\n"


ftn4 = " Single-origin^[Some remark.] coffee farm-to-table tattooed 90's, brunch kinfolk pinterest freegan deep v mumblecore green juice retro. Brooklyn^[In New York!] kinfolk venmo cloud bread, helvetica raw denim cliche 3 wolf moon microdosing brunch stumptown gentrify messenger bag^[Nice. What more?] lomo. Fam cliche listicle wolf, quinoa poke venmo YOLO.^[Blah blah blah. End of footnote.]"

ftn5 = "^[Lastly,\n a short footnote.]"

ftn6 = "[^ftn1]: Food truck jean shorts organic activated charcoal quinoa. Yuccie health goth swag, man bun PBR&B farm-to-table tumeric four loko pok pok sriracha. Squid locavore tumeric man bun, mumblecore blue bottle schlitz. Snackwave mlkshk 3 wolf moon chillwave bicycle rights gastropub migas gentrify vice cold-pressed church-key scenester art party squid jianbing. Sustainable chicharrones pour-over, ugh 90's austin freegan kitsch blue bottle taxidermy wayfarers poutine photo booth leggings migas. Heirloom single-origin coffee +1 waistcoat vinyl hoodie green juice enamel pin distillery etsy.\n\n  Man bun aesthetic godard echo park gentrify. Vape hot chicken flannel, post-ironic kale chips messenger bag street art thundercats dreamcatcher seitan mixtape mustache shoreditch roof party. Kitsch irony occupy, tofu kogi mustache hashtag migas.\n\n  Vape butcher 3 wolf moon beard knausgaard wolf trust fund kinfolk ethical slow-carb. Ugh ethical tacos letterpress, seitan ennui hoodie salvia. Tumeric pabst bushwick vaporware pinterest.\n\n"


pftn1 = runParserT footnoteP "" ftn1
pftn2 = runParserT footnoteB "" ftn2
pftn3 = runParserT footnoteB "" ftn3
pftn4 = runParserT block "" ftn4
pftn5 = runParserT footnoteP "" ftn5
pftn6 = runParserT footnoteB "" ftn6


-- Comments --
--------------

com0 = " <!-- Four loko quinoa etsy selvage marfa PBR&B fingerstache kale chips tattooed seitan. --> "

com1 = " Four <!-- loko --> quinoa etsy selvage marfa PBR&B <!-- fingerstache kale --> chips tattooed seitan. "

com2 = "Sustainable semiotics man bun fashion axe celiac thundercats. Tacos williamsburg four loko kickstarter bespoke. <!-- Four loko quinoa etsy selvage marfa PBR&B fingerstache kale chips tattooed seitan. --> Lumbersexual subway tile drinking vinegar venmo pour-over hell of leggings sustainable ennui hot chicken. Poke austin put a bird on it neutra chia. Adaptogen poke plaid twee whatever umami kale chips hell of crucifix yr tumblr godard. Ugh meh gluten-free lo-fi, dreamcatcher live-edge scenester aesthetic hashtag. Snackwave etsy twee echo park, sartorial copper mug meditation tofu YOLO affogato. <!-- Live-edge hashtag bespoke, post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag.-->"
com3 = "<!--Sustainable semiotics man bun fashion axe celiac thundercats. Tacos williamsburg four loko kickstarter bespoke. Four loko quinoa etsy selvage marfa PBR&B fingerstache kale chips tattooed seitan. Lumbersexual subway tile drinking vinegar venmo pour-over hell of leggings sustainable ennui hot chicken. Poke austin put a bird on it neutra chia. Adaptogen poke plaid twee whatever umami kale chips hell of crucifix yr tumblr godard. Ugh meh gluten-free lo-fi, dreamcatcher live-edge scenester aesthetic hashtag. Snackwave etsy twee echo park, sartorial copper mug meditation tofu YOLO affogato. Live-edge hashtag bespoke, post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag.\n-->"
com4 = "<!-- Sustainable semiotics man bun fashion axe celiac thundercats.\n Tacos williamsburg four loko kickstarter bespoke.\n Four loko quinoa etsy selvage marfa PBR&B fingerstache kale chips tattooed seitan. Lumbersexual subway tile drinking vinegar venmo pour-over hell of leggings sustainable ennui hot chicken. Poke austin put a bird on it neutra chia.\n Adaptogen poke plaid twee whatever umami kale chips hell of crucifix yr tumblr godard. Ugh meh gluten-free lo-fi, dreamcatcher live-edge scenester aesthetic hashtag. Snackwave etsy twee echo park, sartorial copper mug meditation tofu YOLO affogato. Live-edge hashtag bespoke, post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag.\n\nPolaroid gochujang whatever,\n deep v four dollar toast kitsch authentic +1 banjo poke synth single-origin coffee DIY gluten-free.\n Paleo ugh retro, letterpress skateboard dreamcatcher blog.\n Truffaut biodiesel sartorial activated charcoal, cred direct trade cray iPhone.\n Chia leggings narwhal green juice aesthetic migas fashion axe coloring book distillery keytar fingerstache ennui heirloom gochujang. -->"
com5 = "Live-edge hashtag bespoke, <!-- comment here --> post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag."
com6 = "Live-edge hashtag bespoke, post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag. <!-- Live-edge hashtag bespoke, post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag. -->"
com7 = " <!-- Live-edge hashtag bespoke, post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag.-->"


pcom0 = runParserT paraComment "" com0
pcom1 = runParserT sentence "" com1
pcom2 = runParserT block "" com2
pcom3 = runParserT block "" com3
pcom4 = runParserT block "" com4
pcom5 = runParserT sentence "" com5
pcom6 = runParserT block "" com6
pcom7 = runParserT someParaPart "" com7

