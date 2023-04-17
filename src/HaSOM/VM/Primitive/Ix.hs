{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Collection of all indexing VM types
module HaSOM.VM.Primitive.Ix
  ( ObjIx,
    InsIx,
    SymbolIx,
    FieldIx,
    GlobalIx,
    VMIx (..),
  )
where

import Data.Ix (Ix)

-- | Unique object id
newtype ObjIx = MkObjIx {getObjIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord)

-- | Unique symbol id
newtype SymbolIx = MkSymbolIx {getSymbolIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord)

----------------------------------

-- | Instruction pointer
newtype InsIx = MkInsIx {getInsIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord)

----------------------------------

-- | Index in field
newtype FieldIx = MkFieldIx {getFieldIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord)

-- | Global symbols index
newtype GlobalIx = MkGlobalIx {getGlobalIx :: Int}
  deriving newtype (Show, Bounded, Enum, Ix, Num, Real, Integral, Eq, Ord)

----------------------------------
class VMIx ix where
  ix :: Int -> ix
  getIx :: ix -> Int
  changeIx :: (VMIx ix') => ix -> ix'
  changeIx = ix . getIx

instance VMIx ObjIx where
  ix = MkObjIx
  getIx = getObjIx

instance VMIx SymbolIx where
  ix = MkSymbolIx
  getIx = getSymbolIx

instance VMIx InsIx where
  ix = MkInsIx
  getIx = getInsIx

instance VMIx FieldIx where
  ix = MkFieldIx
  getIx = getFieldIx

instance VMIx GlobalIx where
  ix = MkGlobalIx
  getIx = getGlobalIx
