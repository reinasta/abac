{-# LANGUAGE OverloadedStrings #-}
module Abac.PartsOfSpeechSpec where

import Test.Hspec hiding (Example)

import Abac.PartsOfSpeech


import Abac.Types
import Abac.Traverse

import qualified Data.Text as T
import qualified Data.Map as M


import Text.Megaparsec hiding (count)
import Abac.Parser (doc,withoutAbbreviations')
import Abac.Tutorials.Texts (pdoc2_res,pdoc5_res,pblc551_res)

spec :: Spec
spec = describe "Filters" $ do

  it "returns words that are both italics and enclosed in parentheses" $ do
    multiflag1_test1 `shouldBe` [Word [Parenthetical,Emph] (2,47) "aperiam"]

  it "returns bolded italc words" $ do
    multiflag1_test2 `shouldBe` [Word [Bold,Emph] (1,23) "unde"]

  it "returns bolded words enclosed in brackets" $ do
    multiflag1_test3 `shouldBe` [ Word [Bracketed,Bold] (3,27) "inventore"
                                , Word [Bracketed,Bold] (3,37) "veritatis"]




-- possible bug: abac-exe -f style_test.md --italic --paren -p 1
multiflag1 :: [Char]
multiflag1 = "**Sed ut perspiciatis *unde* omnis iste natus error sit** voluptatem\naccusantium doloremque laudantium, totam (rem *aperiam*), \n[eaque ipsa quae ab illo **inventore veritatis** et quasi architecto \nbeatae vitae dicta sunt explicabo]."

-- removed wrapping in ListResult and Inlines
multiflag1_parse = [Word [Bold] (1,2) "sed",Word [Bold] (1,6) "ut",Word [Bold] (1,9) "perspiciatis",Word [Bold,Emph] (1,23) "unde",Word [Bold] (1,29) "omnis",Word [Bold] (1,35) "iste",Word [Bold] (1,40) "natus",Word [Bold] (1,46) "error",Word [Bold] (1,52) "sit",Word [None] (1,58) "voluptatem",Word [None] (2,0) "accusantium",Word [None] (2,12) "doloremque",Word [None] (2,23) "laudantium",Word [None] (2,35) "totam",Word [Parenthetical] (2,42) "rem",Word [Parenthetical,Emph] (2,47) "aperiam",Word [Bracketed] (3,1) "eaque",Word [Bracketed] (3,7) "ipsa",Word [Bracketed] (3,12) "quae",Word [Bracketed] (3,17) "ab",Word [Bracketed] (3,20) "illo",Word [Bracketed,Bold] (3,27) "inventore",Word [Bracketed,Bold] (3,37) "veritatis",Word [Bracketed] (3,49) "et",Word [Bracketed] (3,52) "quasi",Word [Bracketed] (3,58) "architecto",Word [Bracketed] (4,0) "beatae",Word [Bracketed] (4,7) "vitae",Word [Bracketed] (4,13) "dicta",Word [Bracketed] (4,19) "sunt",Word [Bracketed] (4,24) "explicabo"]

multiflag1_test1 = filter (`hasAttrs` [Emph,Parenthetical]) multiflag1_parse
multiflag1_test2 = filter (`hasAttrs` [Emph,Bold]) multiflag1_parse
multiflag1_test3 = filter (`hasAttrs` [Bold,Bracketed]) multiflag1_parse




-- markdown inline filters

myexp1 :: Abac PercentResult
myexp1 = percent =<< preposition =<< line 2 =<< paragraph 2 =<< return pdoc2_res

myexp2 :: Abac CountResult
myexp2 = count =<< adword =<< line 9 =<< return pdoc2_res

myexp3 :: Abac ListResult
myexp3 = list =<< weakverb =<< line 3 =<< return pdoc2_res

myexp4 :: Abac PercentResult
myexp4 = percent =<< adword =<< line 3 =<< paragraph 2 =<< return pdoc2_res

myexp5 :: Abac PercentResult
myexp5 = percent =<< weakverb =<< line 4 =<< paragraph 3 =<< return pdoc2_res

myexp6 :: Abac CountResult
myexp6 = count =<< connective =<< line 3 =<< paragraph 5 =<< return pdoc2_res

myexp7 :: Abac ListResult
myexp7 = list =<< indexical =<< line 1 =<< paragraph 1 =<< return pdoc2_res

myexp8 :: Abac CountResult
myexp8 = count =<< nominalisation =<< paragraph 4 =<< return pdoc2_res

myexp9 :: Abac PercentResult
myexp9 = percent =<< prepexpr 2 =<< paragraph 1 =<< return pdoc2_res

myexp10 :: Abac CountResult
myexp10 = count =<< weakverb =<< line 3 =<< return pdoc2_res

myexp11 :: Abac ListResult
myexp11 = list =<< preposition =<< paragraph 1 =<< return pdoc2_res

myexp12 :: Abac ListResult
myexp12 = list =<< preposition =<< paragraph 1 =<< section "2" =<< return pdoc2_res

myexp13 :: Abac PercentResult
myexp13 = percent =<< preposition =<< section "1" =<< return pdoc2_res

myexp14 :: Abac CountResult
myexp14 = count =<< anyword =<< section "1" =<< return pdoc2_res

myexp15 :: Abac ListResult
myexp15 = list =<< email =<< section "1" =<< return pdoc5_res

myexp16 :: Abac CountResult
myexp16 = count =<< citation =<< section "1" =<< return pdoc5_res

myexp17 :: Abac PercentResult
myexp17 = percent =<< emph' =<< section "1" =<< return pdoc5_res

myexp18 :: Abac CountResult
myexp18 = count =<< bold' =<< section "1" =<< return pdoc5_res

myexp19 :: Abac ListResult
myexp19 = list =<< tagged (Tag "VBN") =<< section "1" =<< return pdoc5_res

myexp20 :: Abac PercentResult
myexp20 = percent =<< number' =<< section "1" =<< return pdoc5_res

myexp21 :: Abac ListResult
myexp21 = list =<< sngquoted =<< section "1" =<< return pdoc5_res

myexp22 :: Abac ListResult
myexp22 = list =<< bracketed =<< section "1" =<< return pdoc5_res

myexp23 :: Abac ListResult
myexp23 = list =<< parened =<< section "1" =<< return pdoc5_res

myexp24 :: Abac CountResult
myexp24 = count =<< anyword =<< line 2 =<< return pblc551_res

myexp25 :: Abac ListResult
myexp25 = list =<< anyword =<< section "meta" =<< return pdoc5_res

myexp26 :: Abac TimeResult
myexp26 = time =<< anyword =<< section "meta" =<< return pdoc5_res

some_meta1 :: Section
some_meta1 = Meta (M.fromList [(AuthorKey,MetaInlines [Word [Tag "NN",None] (3,8) "nicolo",Space,Word [Tag "NN",None] (3,15) "malichiavi"]),(TitleKey,MetaInlines [Word [Tag "DT",None] (2,7) "the",Space,Word [Tag "NN",None] (2,11) "princess"]),(OtherKey "note",MetaInlines [Word [Tag "DT",None] (5,6) "a",Space,Word [Tag "NN",None] (5,8) "word",Space,Word [Tag "NN",None] (5,13) "salad",Space,Word [Tag "IN",None] (5,19) "with",Space,Word [Tag "NNS",None] (5,24) "citations",Space,Word [Tag "FW",None] (5,34) "etc"]),(OtherKey "translator",MetaInlines [Word [Tag "NN",None] (4,12) "john",Space,Word [Tag "NN",None] (4,17) "doe"])])

-- passive ngrams test
pmoby' :: IO (Either Err ListResult)
pmoby' = do
  txt <- readFile "docs/examples/mobydick.md"
  Right dc <- runParserT doc "" (withoutAbbreviations' txt)
  fmap resElems <$> runAbc (passiveNgrams 7 dc)

