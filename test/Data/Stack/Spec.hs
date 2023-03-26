{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Stack.Spec (spec) where

import Data.Stack
import System.Random (Random)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

instance (Random a, Arbitrary a) => Arbitrary (Stack a) where
  arbitrary = do
    (x :: [a]) <- listOf chooseAny
    pure (foldr push emptyStack x)

---------------------------------------------------------

emptyStack' :: Stack Int
emptyStack' = emptyStack

exampleList :: [Int]
exampleList = [0, 1, 2, 3, 4, 5, 6]

exampleStack :: Stack Int
exampleStack = foldl (flip push) emptyStack exampleList

---------------------------------------------------------

spec :: Spec
spec = describe "Stack data type" $ do
  it "Popping an empty stack returns nothing" $ do
    pop emptyStack' `shouldBe` Nothing
    popSt emptyStack' `shouldBe` Nothing
    popn 1 emptyStack' `shouldBe` Nothing

  it "Top on empty stack returns nothing" $
    top emptyStack' `shouldBe` Nothing

  it "Size of an empty stack is zero" $
    size emptyStack `shouldBe` 0

  it "getAt returns correctly" $ do
    mapM_ (\ix -> getAt ix exampleStack `shouldBe` Just ix) exampleList
    getAt (-1) exampleStack `shouldBe` Nothing
    getAt 7 exampleStack `shouldBe` Nothing

  it "reserve pushes correct elements" $ do
    let st = reserve 3 (-1) exampleStack
    getAt 7 st `shouldBe` Just (-1)
    getAt 8 st `shouldBe` Just (-1)
    getAt 9 st `shouldBe` Just (-1)
    getAt 10 st `shouldBe` Nothing

  -- Props
  prop "FIFO behaviour on push-pop" prop_PushPop
  prop "FIFO behaviour on push-top" prop_TopLast
  prop "pop, popSt and top behave the same" prop_TopAccess
  prop "popn can pop up to the size of stack" prop_PopNTimes
  prop "popn 0 is identity" prop_PopNId

  prop "reserve changes the size by given size" prop_ReserveSize
  prop "reserve and popn behave as identity" prop_ReservePopnId

---------------------------------------------------------

prop_PushPop :: Int -> Stack Int -> Bool
prop_PushPop i st = pop (push i st) == Just (st, i)

prop_TopLast :: Int -> Stack Int -> Bool
prop_TopLast i st = top (push i st) == Just i

prop_TopAccess :: Stack Int -> Bool
prop_TopAccess st =
  (fst <$> pop st) == popSt st
    && (snd <$> pop st) == top st

prop_PopNTimes :: Stack Int -> Bool
prop_PopNTimes st = popn (size st) st == Just emptyStack

prop_PopNId :: Stack Int -> Bool
prop_PopNId st = popn 0 st == Just st

prop_ReserveSize :: Int -> Stack Int -> Bool
prop_ReserveSize count st
  | count >= 0 = (size st + count) == reservedSize
  | otherwise = size st == reservedSize
  where
    reservedSize = size (reserve count 0 st)

prop_ReservePopnId :: Int -> Stack Int -> Bool
prop_ReservePopnId count st = popn count (reserve count 0 st) == Just st