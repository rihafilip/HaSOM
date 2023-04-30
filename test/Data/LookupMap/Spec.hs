{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.LookupMap.Spec(spec) where

import qualified Data.LookupMap as LM
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Helper.VMIx (TestIx)
import Data.Hashable (Hashable)
import Data.Foldable (foldlM)
import Control.Monad (void)
import Data.List (nub)
import Data.Maybe (isJust)

instance (Arbitrary k, Eq k, Hashable k) => Arbitrary (LM.LookupMap k TestIx) where
    arbitrary = do
      x <- listOf arbitrary
      pure (LM.fromList x)

spec :: Spec
spec =
  describe "LookupMap data type" $ do
    prop "After getOrSet, subsequent get or getOrSet return the same idx" prop_SetAndGet
    prop "new index for each new key" prop_UniqueIx
    prop "putAll puts unique indices" prop_PutAll
    prop "fromList puts unique indices" prop_FromList

prop_SetAndGet :: String -> LM.LookupMap String TestIx -> Expectation
prop_SetAndGet val lmap = do
  let (lmap', idx) = LM.getOrSet val lmap
  LM.get val lmap' `shouldBe` Just idx
  snd (LM.getOrSet val lmap') `shouldBe` idx

prop_UniqueIx :: [String] -> Expectation
prop_UniqueIx vals = void $ foldlM f ([], initLmap) (nub vals)
  where
    initLmap :: LM.LookupMap String TestIx
    initLmap = LM.new
    f (acc, lmap) val = do
      let (lmap', idx) = LM.getOrSet val lmap
      acc `shouldNotContain` [idx]
      pure ( idx : acc, lmap' )

prop_PutAll :: [String] -> Expectation
prop_PutAll vals = validateUnique lmap (nub vals)
  where
    lmap = LM.putAll vals LM.new

prop_FromList :: [String] -> Expectation
prop_FromList vals = validateUnique (LM.fromList vals) (nub vals)

validateUnique :: LM.LookupMap String TestIx -> [String] -> Expectation
validateUnique lmap = void . foldlM f []
  where
    f acc val = do
      LM.get val lmap `shouldSatisfy` isJust
      acc `shouldNotContain` [val]
      pure (val : acc)
