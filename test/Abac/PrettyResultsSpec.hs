{-# LANGUAGE OverloadedStrings #-}
module Abac.PrettyResultsSpec where

import Test.Hspec hiding (Example)

import Abac.PartsOfSpeech


import Abac.Types
import Abac.Traverse

import qualified Data.Text as T
import qualified Data.Map as M



spec :: Spec
spec = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [1,2,3] `shouldBe` (1 :: Int)


--tests
exprs_test1 :: Expressions
exprs_test1 = Expressions [Expression [Word [None] (11,19) "in",Word [None] (11,22) "my",Word [None] (11,25) "purse",Word [None] (11,32) "and",Word [None] (11,36) "nothing",Word [None] (11,44) "particular",Word [None] (11,55) "to"],Expression [Word [None] (11,32) "and",Word [None] (11,36) "nothing",Word [None] (11,44) "particular",Word [None] (11,55) "to",Word [None] (11,58) "interest",Word [None] (11,67) "me",Word [None] (11,70) "on"],Expression [Word [None] (11,36) "nothing",Word [None] (11,44) "particular",Word [None] (11,55) "to",Word [None] (11,58) "interest",Word [None] (11,67) "me",Word [None] (11,70) "on",Word [None] (12,0) "shore"],Expression [Word [None] (11,44) "particular",Word [None] (11,55) "to",Word [None] (11,58) "interest",Word [None] (11,67) "me",Word [None] (11,70) "on",Word [None] (12,0) "shore",Word [None] (12,7) "i"],Expression [Word [None] (11,55) "to",Word [None] (11,58) "interest",Word [None] (11,67) "me",Word [None] (11,70) "on",Word [None] (12,0) "shore",Word [None] (12,7) "i",Word [None] (12,9) "thought"],Expression [Word [None] (18,0) "such",Word [None] (18,5) "an",Word [None] (18,8) "upper",Word [None] (18,14) "hand",Word [None] (18,19) "of",Word [None] (18,22) "me",Word [None] (18,26) "that"],Expression [Word [None] (18,5) "an",Word [None] (18,8) "upper",Word [None] (18,14) "hand",Word [None] (18,19) "of",Word [None] (18,22) "me",Word [None] (18,26) "that",Word [None] (18,31) "it"],Expression [Word [None] (18,8) "upper",Word [None] (18,14) "hand",Word [None] (18,19) "of",Word [None] (18,22) "me",Word [None] (18,26) "that",Word [None] (18,31) "it",Word [None] (18,34) "requires"],Expression [Word [None] (18,14) "hand",Word [None] (18,19) "of",Word [None] (18,22) "me",Word [None] (18,26) "that",Word [None] (18,31) "it",Word [None] (18,34) "requires",Word [None] (18,43) "a"],Expression [Word [None] (18,19) "of",Word [None] (18,22) "me",Word [None] (18,26) "that",Word [None] (18,31) "it",Word [None] (18,34) "requires",Word [None] (18,43) "a",Word [None] (18,45) "strong"]]

expr_test1 :: Expression
expr_test1 = Expression [Word [None] (18,0) "such",Word [None] (18,5) "an",Word [None] (18,8) "upper",Word [None] (18,14) "hand",Word [None] (18,19) "of",Word [None] (18,22) "me",Word [None] (18,26) "that"]

inls_test1 :: Inlines
inls_test1 = Inlines [Word [None] (10,0) "call",Word [None] (10,5) "me",Word [None] (10,8) "ishmael",Word [None] (10,17) "some",Word [None] (10,22) "years",Word [None] (10,28) "ago",Word [None] (10,32) "never",Word [None] (10,38) "mind",Word [None] (10,43) "how",Word [None] (10,47) "long",Word [None] (10,52) "precisely",Word [None] (10,62) "having"]

