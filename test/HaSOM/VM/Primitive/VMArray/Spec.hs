{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HaSOM.VM.Object.VMArray.Spec (spec) where

import HaSOM.VM.Object.VMArray
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Control.Monad (zipWithM_)

instance (Arbitrary a) => Arbitrary (VMArray a) where
  arbitrary = do
    (x :: [a]) <- listOf arbitrary
    pure (fromListArray x)

exampleList :: [Int]
exampleList = [0, 1, 2, 3, 4, 5]

exampleArray :: VMArray Int
exampleArray = fromListArray exampleList

spec :: Spec
spec =
  describe "VMArray data type" $ do
    it "Out of bounds access on array returns nothing" $ do
      getArray exampleArray (-1) `shouldBe` Nothing
      getArray exampleArray 6 `shouldBe` Nothing

    it "Setting an element on array modifies the correct element" $ do
      let arr1 = setArray exampleArray 2 (-10)
      arr1 `shouldBe` Just (fromListArray [0, 1, -10, 3, 4, 5])
      (arr1 >>= (\a -> setArray a 4 (-20)))
        `shouldBe` Just (fromListArray [0, 1, -10, 3, -20, 5])

    it "Setting an array out of bounds returns Nothing" $ do
      setArray exampleArray (-1) 1 `shouldBe` Nothing
      setArray exampleArray 7 1 `shouldBe` Nothing

    -- Props
    prop "fromList has correct elements" prop_FromListArrayCorrectElements
    prop "appendArray and appendArrayIx behave the same" prop_Appending
    prop "newArray returns Array filled with given element" prop_NewArray
    prop "appendArrayIx returns unique indices" prop_UniqueIx

prop_FromListArrayCorrectElements :: [Int] -> Expectation
prop_FromListArrayCorrectElements xs =
  let ar = fromListArray xs
   in zipWithM_ (\el i -> getArray ar i `shouldBe` Just el) xs [0 ..]

prop_Appending :: Int -> VMArray Int -> Expectation
prop_Appending el arr =
  let (arr1, _) = appendArrayIx arr el
      arr2 = appendArray arr el
   in arr1 `shouldBe` arr2

prop_NewArray :: Int -> Char -> Expectation
prop_NewArray count el = mapM_ (\ix -> Just el `shouldBe` getArray arr ix) [0..(count - 1)]
  where
    arr = newArray count el

prop_UniqueIx :: [Int] -> VMArray Int -> Expectation
prop_UniqueIx xs initArr =
  let (res, _, _) = foldr f1 (pure (), [], initArr) xs
      f1 x (expects, indicies, arr) = (ex >> expects, newIx : indicies, arr')
        where
          (arr', newIx) = appendArrayIx arr x
          ex = indicies `shouldNotContain` [newIx]
   in res
