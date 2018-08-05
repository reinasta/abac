{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Megaparsec
import qualified Data.Text as T
import Criterion
import Criterion.Main (bgroup, defaultMain)

import Abac.Parser


main :: IO ()
main = defaultMain
    [ bgroup "Doc parser" benchmarks
    ]


benchmarks :: [Benchmark]
benchmarks =
    [ bench "parse doc1" (nf (parse doc "") doc1 )
    ]


doc1 = T.unpack . T.concat $ (`T.append` "\n\n") <$> [secText1, ftn3, eex1, com4]
doc2 = replicate 10 . T.unpack . T.concat $ (`T.append` "\n\n") <$> [secText, chmText]


-- texts

chmText = "### Introduction\n\n\n\nHe is edging *closer to becoming an octogenarian linguist* and *political activist,* and bound to remember the public birthday wishes he received from his wife Carol when he turned 70: ‘Well, seventy is nice, but what I’m really looking forward to is eighty!’\n\n\nOn his birthday, as on every other day of the year, he receives some 200 e-mails deal-ing with linguistics, politics and other matters.\n\n\nNoam Chomsky is one of the most notable contemporary champions\nof the people. He is also a scientist of the highest calibre. But\nis he great material for a biography? Certainly not, if you ask the\nsubject.\n\n\n#### Next section with a title\nAn intensely private man, he is horrified to be considered \nthe main character in any story. He jokes about the notion\nthat people come to see him, listen to him, even adore him, when\nin fact he is the most boring speaker ever to hit the stage. He gets\nserious very quickly and tells his audiences that they have come\nto hear about the ‘issues’ of our time, issues that are important to\nthem and, as it happens, to him.\n  \n \nWhat is it that he knows and the\npeople don’t?\n\nWrong question, he would say. The people merely\nwant to know the truth and they know it is hidden from them by a\nvast propaganda machine. His skill is to lift the veil and reveal the\ntruth. Anyone can do it, says Chomsky, it only takes some dedicated\nresearch and logical reasoning."

secText ="# Title 1 0\n\nBlock in section 1.0.\n\n## Title 1 1\n\nBlock in section 1.1.\n\n### Title 1 1 1\n\nBlock in section 1 1.1.\n\n####Title 1 1 1 1\n\nBlock in section 1 1 1.1.\n\n##Title 1 2\n\nBlock in section 1.2.\n\n## Title 1 3\n\nBlock in section 1.3.\n\n###Title 1 3 1\n\nBlock in section 1 3 1.\n\n# Title 2\n\nBlock in section 2."

secText1 ="# Title 1 0\n\nBlock in section 1.0.\n\n## Title 1 1\n\nBlock in section 1.1.\n\n### Title 1 1 1\n\nBlock in section 1 1.1.\n\n####Title 1 1 1 1\n\nBlock in section 1 1 1.1.\n\n##Title 1 2\n\nBlock in section 1.2.\n\n## Title 1 3\n\nBlock in section 1.3.\n\n###Title 1 3 1\n\nBlock in section 1 3 1.\n\n\n####Title 1 3 1 1.\nThis is a paragraph in section 1 3 1 1.\n\n###Title 1 3 2\n\nThis is another paragraph!\n\n# Title 2\n\nBlock in section 2."

ftn3 = "[^ftn45]: Kitsch irony occupy, tofu kogi mustache hashtag migas.^[A short footnote inside a big footnote.] Vape butcher 3 wolf moon beard knausgaard wolf trust fund kinfolk ethical slow-carb. An example:\n(1) The first example.\n  a. Sub-example blah 1.\n  b. Sub-example blah 2.\n(@2) Another main example.\nUgh ethical tacos letterpress, seitan ennui hoodie salvia. Tumeric pabst bushwick vaporware pinterest.\n\n"

eex1 ="\n1. Tote bag glossier knausgaard messenger bag put a bird on it.\n  a. Lumbersexual schlitz ramps meh retro? Mlkshk hammock sriracha crucifix fingerstache.^[Remark.] Blah.\n    b. Yuccie health goth venmo iceland pinterest echo park pabst viral deep v brunch +1 twee beard.^[Another remark. A good one.] Blah!\n      i. Green juice drinking vinegar cold-pressed retro twee church-key meh. \n        ii. Iceland cardigan keffiyeh; meh venmo DIY cloud.^[Note this note.] Bread enamel pin paleo echo park pug.\n    c. Blah blah lumbersexual schlitz ramps retro.\n       i. Iceland cardigan keffiyeh. Meh venmo DIY!^[Note!] Cloud bread enamel pin paleo echo park pug.\n         ii. Readymade adaptogen PBR&B locavore hot chicken tattooed umami.^[A short note. Read on!]\n   b. 8-bit poke blog gochujang.^[With a note!] Offal gentrify kombucha etsy lomo ethical hexagon wayfarers gluten-free hot chicken fashion axe roof party vaporware plaid taiyaki.\n2. Coming back to the first level.^[It's been so long, this travel through our subexample-scape!]\n  a. Etsy everyday carry kombucha master cleanse, kickstarter freegan tbh swag hella synth lyft venmo knausgaard fashion axe.^[Note. Short. Like it!]\n  b. Salvia +1 polaroid mlkshk williamsburg. Tacos cronut lo-fi hella sartorial authentic activated charcoal.^[Note: here is the note!] Craft beer portland swag fam skateboard hot chicken fanny pack.\n\n"

com4 = "<!-- Sustainable semiotics man bun fashion axe celiac thundercats.\n Tacos williamsburg four loko kickstarter bespoke.\n Four loko quinoa etsy selvage marfa PBR&B fingerstache kale chips tattooed seitan. Lumbersexual subway tile drinking vinegar venmo pour-over hell of leggings sustainable ennui hot chicken. Poke austin put a bird on it neutra chia.\n Adaptogen poke plaid twee whatever umami kale chips hell of crucifix yr tumblr godard. Ugh meh gluten-free lo-fi, dreamcatcher live-edge scenester aesthetic hashtag. Snackwave etsy twee echo park, sartorial copper mug meditation tofu YOLO affogato. Live-edge hashtag bespoke, post-ironic drinking vinegar lo-fi 90's poutine twee squid crucifix kogi humblebrag.\n\nPolaroid gochujang whatever,\n deep v four dollar toast kitsch authentic +1 banjo poke synth single-origin coffee DIY gluten-free.\n Paleo ugh retro, letterpress skateboard dreamcatcher blog.\n Truffaut biodiesel sartorial activated charcoal, cred direct trade cray iPhone.\n Chia leggings narwhal green juice aesthetic migas fashion axe coloring book distillery keytar fingerstache ennui heirloom gochujang. -->"
