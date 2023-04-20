{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Definition of VM Universe
module HaSOM.VM.Universe
  ( -- * TODO doc
    VMObjectNat,
    VMClassNat,
    VMMethodNat,
    VMGlobalsNat,

    -- * Call stack item
    CallFrame (..),

    -- * Type definitions of VM data
    ObjStack,
    CallStack,
    Literals,
    GCNat,

    -- * Universe effect definitions
    ObjStackEff,
    GlobalsEff,
    CallStackEff,
    LiteralEff,
    GCEff,
    UniverseEff,

    -- * Primitive function definitions
    NativeFun,
    mkNativeFun,
    runNativeFun,
  )
where

import Control.Eff (Eff, type (<::))
import Control.Eff.ExcT (ExcT)
import Control.Eff.Extend (Member)
import Control.Eff.State.Strict (State)
import Data.List.NonEmpty (NonEmpty)
import Data.Stack (Stack)
import HaSOM.VM.GC (GC)
import HaSOM.VM.Object

-----------------------------------

type VMObjectNat = VMObject NativeFun

type VMClassNat = VMClass NativeFun

type VMMethodNat = VMMethod NativeFun

type VMGlobalsNat = VMGlobals NativeFun

type GCNat = GC VMObjectNat

-----------------------------------

-- | Call stack item
data CallFrame = MkCallFrame
  { stackOffset :: Int,
    method :: VMMethodNat,
    code :: Code,
    pc :: InsIx,
    this :: ObjIx,
    frameId :: Maybe Int,
    capturedFrame :: Maybe CallFrame
  }

-----------------------------------

-- | VM Working stack
type ObjStack = Stack ObjIx

-- | VM Call Stack
type CallStack = NonEmpty CallFrame

-- | VM Symbols definition
type Literals = VMArray LiteralIx VMLiteral

-----------------------------------

-- | VM Working stack effect
type ObjStackEff r = Member (State ObjStack) r

-- | VM Classes effect
type GlobalsEff r = Member (State VMGlobalsNat) r

-- | VM Call Stack effect
type CallStackEff r = Member (State CallStack) r

-- | VM Symbols definition effect
type LiteralEff r = Member (State Literals) r

-- | VM GC definition effect
type GCEff r = Member (State GCNat) r

-----------------------------------

-- | Whole VM effect
type UniverseEff r =
  [ State ObjStack,
    State VMGlobalsNat,
    State CallStack,
    State Literals,
    State GCNat,
    ExcT
  ]
    <:: r

-----------------------------------

-- | Native function polymorphic wrapper,
-- needed for cyclical dependency between Universe and NativeFun
newtype NativeFun = MkNativeFun (forall r. (UniverseEff r) => Eff r ())

-- | Create a native function
mkNativeFun :: (forall r. (UniverseEff r) => Eff r ()) -> NativeFun
mkNativeFun = MkNativeFun

-- | Run a native function
runNativeFun :: UniverseEff r => NativeFun -> Eff r ()
runNativeFun (MkNativeFun f) = f
