{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helper.VMIx(TestIx) where

import Data.Ix (Ix)
import HaSOM.VM.Object (VMIx (..))
import Test.QuickCheck (Arbitrary)

newtype TestIx = MkIx Int
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord, Arbitrary)

instance VMIx TestIx where
  ix = MkIx
  getIx (MkIx i) = i
