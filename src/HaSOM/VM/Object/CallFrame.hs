-- | VM Call frame defintion
module HaSOM.VM.Object.CallFrame (CallFrame (..)) where

import HaSOM.VM.Object.Bytecode
import HaSOM.VM.Object.Ix
import HaSOM.VM.Object.VMMethod

-- | Call stack item,
-- parametrised by native function type
data CallFrame f = MkCallFrame
  { stackOffset :: Int,
    method :: VMMethod f,
    code :: Code,
    pc :: InsIx,
    this :: ObjIx,
    frameId :: Maybe Int,
    capturedFrame :: Maybe (CallFrame f)
  }
