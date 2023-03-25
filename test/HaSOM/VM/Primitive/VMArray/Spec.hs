{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HaSOM.VM.Primitive.VMArray.Spec (spec) where

import HaSOM.VM.Primitive.VMArray
import System.Random (Random)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

instance (Random a, Arbitrary a) => Arbitrary (VMArray a) where
  arbitrary = do
    (x :: [a]) <- listOf chooseAny
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

prop_FromListArrayCorrectElements :: [Int] -> Bool
prop_FromListArrayCorrectElements xs =
  let ar = fromListArray xs
   in and $ zipWith (\el i -> getArray ar i == Just el) xs [0 ..]

prop_Appending :: Int -> VMArray Int -> Bool
prop_Appending el arr =
  let (arr1, _) = appendArrayIx arr el
      arr2 = appendArray arr el
   in arr1 == arr2

prop_NewArray :: Int -> Char -> Bool
prop_NewArray count el = all (\ix -> Just el == getArray arr ix) [0..(count - 1)]
  where
    arr = newArray count el

prop_UniqueIx :: [Int] -> VMArray Int -> Bool
prop_UniqueIx xs initArr =
  let (res, _, _) = foldr f1 (True, [], initArr) xs
      f1 x (b, indicies, arr) = (b' && b, newIx : indicies, arr')
        where
          (arr', newIx) = appendArrayIx arr x
          b' = newIx `notElem` indicies
   in res
