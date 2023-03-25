module Combinator.Spec (spec) where

import Combinator
import Test.Hspec

spec :: Spec
spec =
  describe "Blackbird combinator test" $ do
    it "String test" $
      ((++ "c") ... (++)) "a" "b" `shouldBe` "abc"
    it "Integer test" $
      (show ... div) 10 (2 :: Int) `shouldBe` "5"
