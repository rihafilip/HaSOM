{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HaSOM.VM.VMArray.Spec (spec) where

import Control.Monad (zipWithM_)
import Data.Ix (Ix)
import HaSOM.VM.Object (VMIx (..))
import HaSOM.VM.VMArray (VMArray)
import qualified HaSOM.VM.VMArray as Arr
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    it,
    shouldBe,
    shouldNotContain,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (VMArray i a) where
  arbitrary = do
    (x :: [a]) <- listOf arbitrary
    pure (Arr.fromList x)

newtype TestIx = MkIx Int
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord)

instance VMIx TestIx where
  ix = MkIx
  getIx (MkIx i) = i

exampleList :: [Int]
exampleList = [0, 1, 2, 3, 4, 5]

exampleArray :: VMArray TestIx Int
exampleArray = Arr.fromList exampleList

spec :: Spec
spec =
  describe "VMArray data type" $ do
    it "Out of bounds access on array returns nothing" $ do
      Arr.get (-1) exampleArray `shouldBe` Nothing
      Arr.get 6 exampleArray `shouldBe` Nothing

    it "Setting an element on array modifies the correct element" $ do
      let arr1 = Arr.set 2 (-10) exampleArray
      arr1 `shouldBe` Just (Arr.fromList [0, 1, -10, 3, 4, 5])
      (arr1 >>= Arr.set 4 (-20))
        `shouldBe` Just (Arr.fromList [0, 1, -10, 3, -20, 5])

    it "Setting an array out of bounds returns Nothing" $ do
      Arr.set (-1) 1 exampleArray `shouldBe` Nothing
      Arr.set 7 1 exampleArray `shouldBe` Nothing

    -- Props
    prop "fromList has correct elements" prop_FromListArrayCorrectElements
    prop "appendArray and appendArrayIx behave the same" prop_Appending
    prop "newArray returns Array filled with given element" prop_NewArray
    prop "appendArrayIx returns unique indices" prop_UniqueIx

prop_FromListArrayCorrectElements :: [Int] -> Expectation
prop_FromListArrayCorrectElements xs =
  let ar :: VMArray TestIx Int
      ar = Arr.fromList xs
   in zipWithM_ (\el i -> Arr.get i ar `shouldBe` Just el) xs [0 ..]

prop_Appending :: Int -> VMArray TestIx Int -> Expectation
prop_Appending el arr =
  let (arr1, _) = Arr.appendIx el arr
      arr2 = Arr.append el arr
   in arr1 `shouldBe` arr2

prop_NewArray :: Int -> Char -> Expectation
prop_NewArray count el = mapM_ (\idx -> Just el `shouldBe` Arr.get idx arr) [0 .. (ix count - 1)]
  where
    arr :: VMArray TestIx Char
    arr = Arr.new count el

prop_UniqueIx :: [Int] -> VMArray TestIx Int -> Expectation
prop_UniqueIx xs initArr =
  let (res, _, _) = foldr f1 (pure (), [], initArr) xs
      f1 x (expects, indicies, arr) = (ex >> expects, newIx : indicies, arr')
        where
          (arr', newIx) = Arr.appendIx x arr
          ex = indicies `shouldNotContain` [newIx]
   in res
