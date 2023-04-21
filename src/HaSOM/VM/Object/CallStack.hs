{-# OPTIONS_GHC -Wno-partial-fields #-}
-- | VM Call frame defintion
module HaSOM.VM.Object.CallStack (CallFrame (..), CallStack) where

import qualified Data.Stack as St
import HaSOM.VM.Object.Bytecode (Code)
import HaSOM.VM.Object.Ix
import HaSOM.VM.VMArray (VMArray)

-- | Call stack item,
-- parametrised by native function type
data CallFrame f
  = MethodCallFrame
      { currentCode :: Either f Code,
        pc :: InsIx,
        locals :: VMArray LocalIx ObjIx,
        frameId :: Maybe Int
      }
  | BlockCallFrame
      { currentCode :: Either f Code,
        pc :: InsIx,
        locals :: VMArray LocalIx ObjIx,
        capturedFrame :: CallFrame f
      }

-- | Call stack definition
type CallStack f = St.Stack (CallFrame f)
