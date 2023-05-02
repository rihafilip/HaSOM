{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Definition of VM Universe, specialization of types and effects
module HaSOM.VM.Universe
  ( -- * Tracing flag definition
    Trace(..),
    traceFromBool,

    -- * Native function specialization of parametrized types
    CallFrameNat,
    CallStackItemNat,
    CallStackNat,
    VMClassNat,
    VMGlobalNat,
    VMGlobalsNat,
    VMMethodNat,
    VMObjectNat,
    GCNat,

    -- * Working stack definition
    ObjStack,

    -- * Universe effect definitions
    ObjStackEff,
    GlobalsEff,
    CoreClassesEff,
    CallStackEff,
    LiteralEff,
    GCEff,
    UniverseEff,
    TraceEff,

    -- * Primitive function definitions
    NativeFun,
    mkNativeFun,
    runNativeFun,
  )
where

import Control.Eff (Eff, type (<::), Lifted)
import Control.Eff.ExcT (ExcT)
import Control.Eff.Extend (Member)
import Control.Eff.Reader.Strict (Reader)
import Control.Eff.State.Strict (State)
import Data.Stack (Stack)
import HaSOM.VM.GC (GC)
import HaSOM.VM.Object

-----------------------------------

data Trace = DoTrace | NoTrace

traceFromBool :: Bool -> Trace
traceFromBool True = DoTrace
traceFromBool False = NoTrace

-----------------------------------

-- Call stack
type CallFrameNat = CallFrame NativeFun

type CallStackItemNat = CallStackItem NativeFun

type CallStackNat = CallStack NativeFun

-- Class
type VMClassNat = VMClass NativeFun

-- Global
type VMGlobalNat = VMGlobal NativeFun

type VMGlobalsNat = VMGlobals NativeFun

-- Method
type VMMethodNat = VMMethod NativeFun

-- Object
type VMObjectNat = VMObject NativeFun

-- GC
type GCNat = GC VMObjectNat

-----------------------------------

-- | VM Working stack
type ObjStack = Stack ObjIx

-----------------------------------

-- | VM Working stack effect
type ObjStackEff r = Member (State ObjStack) r

-- | VM Globals effect
type GlobalsEff r = Member (State VMGlobalsNat) r

-- | VM Core classes holder
type CoreClassesEff r = Member (Reader CoreClasses) r

-- | VM Call Stack effect
type CallStackEff r = Member (State CallStackNat) r

-- | VM Symbols definition effect
type LiteralEff r = Member (State VMLiterals) r

-- | VM GC definition effect
type GCEff r = Member (State GCNat) r

-- | VM Trace effect
type TraceEff r = Member (Reader Trace) r

-----------------------------------

-- | Whole VM effect
type UniverseEff r =
  [ State ObjStack,
    State VMGlobalsNat,
    Reader CoreClasses,
    State CallStackNat,
    State VMLiterals,
    State GCNat,
    ExcT
  ]
    <:: r

-----------------------------------

-- | Native function polymorphic wrapper,
-- needed for cyclical dependency between Universe and NativeFun
newtype NativeFun = MkNativeFun (forall r. (UniverseEff r, Lifted IO r) => Eff r ())

-- | Create a native function
mkNativeFun :: (forall r. (UniverseEff r, Lifted IO r) => Eff r ()) -> NativeFun
mkNativeFun = MkNativeFun

-- | Run a native function
runNativeFun :: (UniverseEff r, Lifted IO r) => NativeFun -> Eff r ()
runNativeFun (MkNativeFun f) = f
