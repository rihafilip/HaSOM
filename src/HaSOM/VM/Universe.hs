-- | Definition of VM Universe
module HaSOM.VM.Universe
  ( -- * Call stack item
    CallFrame(..),

    -- * Type definitions of VM data
    ObjStack,
    CallStack,
    Classes,
    Symbols,
  )
where

import Data.List.NonEmpty
import Data.Stack
import HaSOM.Bytecode (Bytecode)
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
type Classes = VMArray VMClass

-- | VM Symbols definition
type Symbols = VMArray VMSymbol
