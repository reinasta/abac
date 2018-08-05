module Abac.Cli.UtilsCliSpec where

import Test.Hspec hiding (Example)

import Abac.Cli.Command

spec :: Spec
spec = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [1,2,3] `shouldBe` (1 :: Int)


{-
-- test
cmmdtest1 = do
  Right doc <- Abac.Parser.pblc551
  --putStrLn $ show doc
  let abc = cmmdToAbac defaultCommand_test Abac.Parser.pdoc2_res -- doc
  eres <- runAbc abc
  putStrLn $ show eres
  --putStrLn $ show defaultCommand_test
  --putStrLn . show =<< runAbc (sequencePosArgs defaultCommand_test $ return doc)
  --sequencePosArgsTest defaultCommand_test (return doc)
-}
