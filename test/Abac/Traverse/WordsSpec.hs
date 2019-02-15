module Abac.Traverse.WordsSpec where

import Test.Hspec hiding (Example)



spec :: Spec
spec = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [1,2,3] `shouldBe` (1 :: Int)


