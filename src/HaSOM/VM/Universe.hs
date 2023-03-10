{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Definition of VM Universe
module HaSOM.VM.Universe
  ( -- * Call stack item
    CallFrame (..),

    -- * Type definitions of VM data
    ObjStack,
    CallStack,
    Classes,
    Symbols,

    -- * Universe effect definitions
    ObjStackEff,
    CallStackEff,
    ClassesEff,
    SymbolsEff,
    GCEff,
    ExcT,
    UniverseEff,

    -- * Primitive function definitions
    NativeFun,
    mkNativeFun,
    runNativeFun,
  )
where

import Control.Eff (Eff, type (<::))
import Control.Eff.Exception (Exc)
import Control.Eff.Extend (Member)
import Control.Eff.State.Strict (State)
import Data.List.NonEmpty (NonEmpty)
import Data.Stack (Stack)
import Data.Text (Text)
import HaSOM.Bytecode (Bytecode)
import HaSOM.VM.GC (GC)
import HaSOM.VM.Primitive

-- | Call stack item
data CallFrame = MkCallFrame
  { stackOffset :: Int,
    classContext :: ClassIx,
    code :: VMArray Bytecode
  }

-----------------------------------

-- | VM Working stack
type ObjStack = Stack ObjIx

-- | VM Call Stack
type CallStack = NonEmpty CallFrame

-- | VM Classes definitions
type Classes = VMArray (VMClass NativeFun)

-- | VM Symbols definition
type Symbols = VMArray VMSymbol

-----------------------------------

-- | VM Working stack effect
type ObjStackEff r = Member (State ObjStack) r

-- | VM Call Stack effect
type CallStackEff r = Member (State CallStack) r

-- | VM Classes definitions effect
type ClassesEff r = Member (State Classes) r

-- | VM Symbols definition effect
type SymbolsEff r = Member (State Symbols) r

-- | VM GC definition effect
type GCEff r = Member (State GC) r

-----------------------------------

-- | VM Exception effect
type ExcT r = Member (Exc Text) r

-- | Whole VM effect
type UniverseEff r =
  [ State ObjStack,
    State CallStack,
    State Classes,
    State Symbols,
    State GC,
    Exc Text
  ]
    <:: r

-----------------------------------

-- | Native function polymorphic wrapper
newtype NativeFun = MkNativeFun (forall r. (UniverseEff r) => Eff r ())

-- | Create a native function
mkNativeFun :: (forall r. (UniverseEff r) => Eff r ()) -> NativeFun
mkNativeFun = MkNativeFun

-- | Run a native function
runNativeFun :: UniverseEff r => NativeFun -> Eff r ()
runNativeFun (MkNativeFun f) = f
