-- | VM Call frame defintion
module HaSOM.VM.Object.CallStack (CallFrame (..), CallStack) where

import HaSOM.VM.Object.Ix
import HaSOM.VM.Object.VMMethod
import HaSOM.VM.VMArray (VMArray)
import qualified Data.Stack as St

-- | Call stack item,
-- parametrised by native function type
data CallFrame f = MkCallFrame
  { stackOffset :: Int,
    method :: VMMethod f,
    pc :: InsIx,
    locals :: VMArray LocalIx ObjIx,
    frameId :: Maybe Int,
    capturedFrame :: Maybe (CallFrame f)
  }

-- | Call stack definition
type CallStack f = St.Stack (CallFrame f)
