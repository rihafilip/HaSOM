{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HaSOM.VM.GC.Spec (spec) where

import HaSOM.VM.GC (GC)
import qualified HaSOM.VM.GC as GC
import HaSOM.VM.Object (ObjIx)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (GC a) where
  arbitrary = do
    (xs :: [a]) <- listOf arbitrary
    (n :: a) <- arbitrary
    pure (foldr f (GC.empty n) xs)
    where
      f x gc =
        let
          (gc', idx) = GC.new gc
        in GC.setAt idx x gc'

instance Show (GC a) where
  show = const "<GC>"

exampleNil :: Int
exampleNil = -10

exampleGC :: GC Int
exampleGC = GC.empty exampleNil

genPos :: Gen Int
genPos = abs <$> (arbitrary :: Gen Int)

addMany :: GC Int -> [Int] -> GC Int
addMany = foldl add
  where
    add gc el =
      let (gc', ix) = GC.new gc
       in GC.setAt ix el gc'

spec :: Spec
spec =
  describe "Garbage Collector" $ do
    it "nil returns the nil object" $ do
      let gc = addMany exampleGC (replicate 7 0)
      GC.getAt (GC.nil gc) gc `shouldBe` Just exampleNil

    it "getAt on empty GC returns Nothing" $
      mapM_
        (\ix -> GC.getAt ix exampleGC `shouldBe` Nothing)
        [-13, -1, 1, 5, 7, 10]

    it "getAt returns the correct element" $ do
      let add gc el = (GC.setAt ix el gc', ix)
            where
              (gc', ix) = GC.new gc
      let (gc1, ix1) = add exampleGC 1
      let (gc2, ix2) = add gc1 2
      let (gc3, ix3) = add gc2 3
      let (gc4, ix4) = add gc3 4
      let (gc5, ix5) = add gc4 5

      GC.nil gc1 `shouldBe` GC.nil exampleGC
      GC.nil gc2 `shouldBe` GC.nil exampleGC
      GC.nil gc3 `shouldBe` GC.nil exampleGC
      GC.nil gc4 `shouldBe` GC.nil exampleGC
      GC.nil gc5 `shouldBe` GC.nil exampleGC

      let allShouldBe ix el = mapM_ (\gc -> GC.getAt ix gc `shouldBe` Just el)

      allShouldBe
        (GC.nil exampleGC)
        exampleNil
        [gc1, gc2, gc3, gc4, gc5]

      allShouldBe
        ix1
        1
        [gc1, gc2, gc3, gc4, gc5]

      allShouldBe
        ix2
        2
        [gc2, gc3, gc4, gc5]

      allShouldBe
        ix3
        3
        [gc3, gc4, gc5]

      allShouldBe
        ix4
        4
        [gc4, gc5]

      allShouldBe
        ix5
        5
        [gc5]

    -- Props
    prop "set and get identity" prop_SetGet
    prop "new returns new index every time and sets to nil" $
      forAll genPos prop_UniqueIx

prop_SetGet :: Int -> GC Int -> Expectation
prop_SetGet value gc =
  let (gc', ix) = GC.new gc
   in GC.getAt ix (GC.setAt ix value gc') `shouldBe` Just value

prop_UniqueIx :: Int -> GC Int -> Expectation
prop_UniqueIx count initGc = r count [GC.nil initGc] initGc
  where
    r :: Int -> [ObjIx] -> GC Int -> Expectation
    r 0 _ _ = pure ()
    r c acc gc = do
      let (gc', ix) = GC.new gc
      GC.getAt ix gc' `shouldBe` GC.getAt (GC.nil gc') gc'
      acc `shouldNotContain` [ix]
      r (c - 1) (ix : acc) gc'
