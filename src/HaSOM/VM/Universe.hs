{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Definition of VM Universe
module HaSOM.VM.Universe
  ( -- * Call stack item
    CallFrame (..),

    -- * Type definitions of VM data
    ObjStack,
    Classes,
    CallStack,
    Symbols,

    -- * Universe effect definitions
    ObjStackEff,
    ClassesEff,
    CallStackEff,
    SymbolsEff,
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
import HaSOM.VM.Primitive

-- | Call stack item
data CallFrame f = MkCallFrame
  { stackOffset :: Int,
    method :: VMMethod f,
    code :: Code,
    pc :: Int
  }

-----------------------------------

-- | VM Working stack
type ObjStack = Stack ObjIx

-- | VM Classes definition
type Classes = VMArray (VMClass NativeFun)

-- | VM Call Stack
type CallStack = NonEmpty (CallFrame NativeFun)

-- | VM Symbols definition
type Symbols = VMArray VMSymbol

-----------------------------------

-- | VM Working stack effect
type ObjStackEff r = Member (State ObjStack) r

-- | VM Classes effect
type ClassesEff r = Member (State Classes) r

-- | VM Call Stack effect
type CallStackEff r = Member (State CallStack) r

-- | VM Symbols definition effect
type SymbolsEff r = Member (State Symbols) r

-- | VM GC definition effect
type GCEff r = Member (State (GC (VMObject NativeFun))) r

-----------------------------------

-- | Whole VM effect
type UniverseEff r =
  [ State ObjStack,
    State Classes,
    State CallStack,
    State Symbols,
    State (GC (VMObject NativeFun)),
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
