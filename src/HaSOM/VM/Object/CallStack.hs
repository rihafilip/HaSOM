{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | VM Call frame defintion
module HaSOM.VM.Object.CallStack
  ( -- * Call frame definition
    CallFrame (..),

    -- * Call stack item definition and manipulation funcitons
    CallStackItem (..),
    getCallFrame,
    promoteCallFrame,
    modifyCallFrame,

    -- * The whole call stack
    CallStack,
  )
where

import Control.Eff (Eff, Lifted, lift)
import Control.Eff.IO.Utility
import Data.IORef (IORef, newIORef)
import qualified Data.Stack as St
import HaSOM.VM.Object.Ix
import HaSOM.VM.Object.VMMethod (VMMethod)
import HaSOM.VM.VMArray (VMArray)

-- | Call frame,
-- parametrised by native function type
data CallFrame f
  = MethodCallFrame
      { method :: VMMethod f,
        pc :: InsIx,
        locals :: VMArray LocalIx ObjIx
      }
  | BlockCallFrame
      { method :: VMMethod f,
        pc :: InsIx,
        locals :: VMArray LocalIx ObjIx,
        capturedFrame :: IORef (CallFrame f)
      }

-- | Call stack item, it is either a pure CallFrame
-- or an IORef to a call frame
data CallStackItem f
  = PureCallFrame (CallFrame f)
  | ReferenceCallFrame (IORef (CallFrame f))

-- | Extractthe call frame from call stack item
getCallFrame :: Lifted IO r => CallStackItem f -> Eff r (CallFrame f)
getCallFrame (PureCallFrame cf) = pure cf
getCallFrame (ReferenceCallFrame cf) = lreadIORef cf

-- | Promote a pure call frame to ReferenceCallFrame
promoteCallFrame :: Lifted IO r => CallStackItem f -> Eff r (CallStackItem f, IORef (CallFrame f))
promoteCallFrame (PureCallFrame cf) = (\rcf -> (ReferenceCallFrame rcf, rcf)) <$> lift (newIORef cf)
promoteCallFrame it@(ReferenceCallFrame cf) = pure (it, cf)

-- | Modify the underlying call frame
modifyCallFrame :: Lifted IO r => CallStackItem f -> (CallFrame f -> Eff r (CallFrame f)) -> Eff r (CallStackItem f)
modifyCallFrame (PureCallFrame cf) f = PureCallFrame <$> f cf
modifyCallFrame it@(ReferenceCallFrame rcf) f = it <$ lmodifyIORef rcf f

-- | Call stack definition
type CallStack f = St.Stack (CallStackItem f)
