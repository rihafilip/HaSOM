{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Collection of all indexing VM types
module HaSOM.VM.Object.Ix
  ( ObjIx,
    InsIx,
    LiteralIx,
    LocalIx,
    FieldIx,
    GlobalIx,
    VMIx (..),
  )
where

import Data.Ix (Ix)
import Data.Hashable (Hashable)

-- | Unique object id
newtype ObjIx = MkObjIx {getObjIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord, Hashable)

-- | Unique symbol id
newtype LiteralIx = MkLiteralIx {getLiteralIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord, Hashable)

----------------------------------

-- | Instruction pointer
newtype InsIx = MkInsIx {getInsIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord, Hashable)

----------------------------------

newtype LocalIx = MkLocalIx {getLocalIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord, Hashable)

-- | Index in field
newtype FieldIx = MkFieldIx {getFieldIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord, Hashable)

-- | Global symbols index
newtype GlobalIx = MkGlobalIx {getGlobalIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord, Hashable)

----------------------------------
class VMIx ix where
  ix :: Int -> ix
  getIx :: ix -> Int
  changeIx :: (VMIx ix') => ix -> ix'
  changeIx = ix . getIx

instance VMIx ObjIx where
  ix = MkObjIx
  getIx = getObjIx

instance VMIx LiteralIx where
  ix = MkLiteralIx
  getIx = getLiteralIx

instance VMIx InsIx where
  ix = MkInsIx
  getIx = getInsIx

instance VMIx FieldIx where
  ix = MkFieldIx
  getIx = getFieldIx

instance VMIx GlobalIx where
  ix = MkGlobalIx
  getIx = getGlobalIx

instance VMIx LocalIx where
  ix = MkLocalIx
  getIx = getLocalIx
