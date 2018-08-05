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
spec = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [1,2,3] `shouldBe` (1 :: Int)


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

