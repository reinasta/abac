module Abac.Cli.CommandSpec where

import Test.Hspec hiding (Example)
import Prelude hiding (abs)

import Abac.Cli.Command

spec :: Spec
spec = describe "Prelude.head" $ do
  it "returns the first element of a list" $ do
    head [1,2,3] `shouldBe` (1 :: Int)


defaultCommand_test :: Command
defaultCommand_test = Command
  { inp       = FileInput defaultStr
  , ind       = ParaIndex 3 -- defaultInt
  , lno       = LineNumber 1
  , sno       = SectNumber ""
  , rep       = ListFlag
  , out       = FileOutput defaultStr
  , other     = Ngram 7
  , aut       = AnyWordFlag
  , ttl       = AnyWordFlag
  , abs       = AnyWordFlag
  , eml       = AnyWordFlag
  , cit       = AnyWordFlag
  , num       = AnyWordFlag
  , math      = AnyWordFlag
  , code      = AnyWordFlag
  , emph      = AnyWordFlag
  , bold      = AnyWordFlag
  , paren     = AnyWordFlag
  , bracket   = AnyWordFlag
  , squote    = AnyWordFlag
  , dquote    = AnyWordFlag
  , adword    = AnyWordFlag
  , connect   = AnyWordFlag
  , indexical = AnyWordFlag
  , nominal   = AnyWordFlag
  , prep      = AnyWordFlag
  , weakvb    = AnyWordFlag
  , passive   = AnyWordFlag
  , anyword   = AnyWordFlag
  , native    = PrettyFlag
  , toc       = NullFlag
  , meta      = NullFlag
  }

