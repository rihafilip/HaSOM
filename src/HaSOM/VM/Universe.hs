{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Definition of VM Universe, specialization of types and effects
module HaSOM.VM.Universe
  ( -- * Tracing flag definition
    Trace(..),
    whenTrace,
    whenTraceGC,

    -- * Run GC flag
    GCFlag(..),

    -- * Execution time start
    RuntimeStartTime(..),

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
    RuntimeStartTimeEff,

    -- * Primitive function definitions
    NativeFun,
    mkNativeFun,
    runNativeFun,
  )
where

import Control.Eff (Eff, type (<::), Lifted)
import Control.Eff.ExcT (ExcT)
import Control.Eff.Extend (Member)
import Control.Eff.Reader.Strict (Reader, ask)
import Control.Eff.State.Strict (State)
import Data.Stack (Stack)
import HaSOM.VM.GC (GC)
import HaSOM.VM.Object
import Data.Time.Clock.System (SystemTime)
import Control.Monad (when)

-----------------------------------

data Trace = MkTrace { execTrace :: Bool, gcTrace :: Bool }

whenTrace :: TraceEff r => Eff r () -> Eff r ()
whenTrace f = ask >>= (`when` f) . execTrace

whenTraceGC :: TraceEff r => Eff r () -> Eff r ()
whenTraceGC f = ask >>= (`when` f) . gcTrace

-----------------------------------

-- | Flag marking if GC should be ran.
-- It cannot be run immidietly, because some object
-- might not be reachable to mark yet
data GCFlag = RunGC | NoGC

-----------------------------------

-- | Time when the Runtime was started,
-- used for determining how long did VM run
newtype RuntimeStartTime =
  MkRuntimeStartTime { runRuntimeStartTime :: SystemTime }

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
type GCEff r = [State GCNat, State GCFlag] <:: r

-- | VM Trace effect
type TraceEff r = Member (Reader Trace) r

-- | VM run time effect
type RuntimeStartTimeEff r = Member (Reader RuntimeStartTime) r

-----------------------------------

-- | Whole VM effect
type UniverseEff r =
  [ State ObjStack,
    State VMGlobalsNat,
    Reader CoreClasses,
    State CallStackNat,
    State VMLiterals,
    State GCNat,
    State GCFlag,
    Reader RuntimeStartTime,
    ExcT
  ]
    <:: r

-----------------------------------

-- | Native function polymorphic wrapper,
-- needed for cyclical dependency between Universe and NativeFun
newtype NativeFun = MkNativeFun (forall r. (UniverseEff r, Lifted IO r) => Eff r (Maybe Int))

-- | Create a native function
mkNativeFun :: (forall r. (UniverseEff r, Lifted IO r) => Eff r (Maybe Int)) -> NativeFun
mkNativeFun = MkNativeFun

-- | Run a native function
runNativeFun :: (UniverseEff r, Lifted IO r) => NativeFun -> Eff r (Maybe Int)
runNativeFun (MkNativeFun f) = f
