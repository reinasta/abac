{-# LANGUAGE OverloadedStrings #-}
module Abac.InternalSpec where

import Test.Hspec hiding (Example)

import Abac.Internal (intersectExpressions,contains)
import Abac.Types.ParserTypes

spec :: Spec
spec = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [1,2,3] `shouldBe` (1 :: Int)



-- testing expression intersection
iexp1, iexp2, iexp3, iexp4 :: Expression
iexp1 = Expression [Word [None] (1,0) "In",Word [None] (1,3) "the",Word [None] (1,7) "middle",Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this"]
iexp2 = Expression [Word [None] (1,3) "the",Word [None] (1,7) "middle",Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this",Word [None] (1,27) "after"]
iexp3 = Expression [Word [None] (1,7) "middle",Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this",Word [None] (1,27) "after",Word [None] (1,33) "all"]
iexp4 = Expression [Word [None] (1,14) "of",Word [None] (1,17) "all",Word [None] (1,21) "this",Word [None] (1,27) "after",Word [None] (1,33) "all",Word [None] (1,37) "that"]
inter12 :: [Inline]
inter12 = intersectExpressions iexp2 iexp3

-- test for `contains`

cexp1 = Expression [Word [None] (0,0) "it",Word [None] (1,0) "is",Word [None] (1,3) "no",Word [None] (1,7) "longer",Word [None] (1,14) "required",Word [None] (1,17) "at",Word [None] (1,21) "all"]
cexp2 = Expression [Word [None] (1,0) "is",Word [None] (1,3) "required"]

tcexp1 = cexp1 `contains` cexp2

