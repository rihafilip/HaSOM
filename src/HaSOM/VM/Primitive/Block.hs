{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.Block (primitives, value) where

import qualified Data.Stack as St
import Data.Text (Text)
import HaSOM.VM.Object
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import qualified HaSOM.VM.VMArray as Arr

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Block",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("value", mkNativeFun value),
    ("restart", mkNativeFun restart)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

value :: (CallStackEff r, ObjStackEff r, Member ExcT r, GCEff r, Lifted IO r) => Eff r ()
value = do
  objIx <- popStack

  (blockCapturedFrame, MkVMBlock {blockBody, blockLocalCount, blockParameterCount}) <-
    getAsObject objIx >>= \case
      BlockObject {blockCapturedFrame, block} -> pure (blockCapturedFrame, block)
      obj -> wrongObjectType obj BlockT

  let method =
        BytecodeMethod
          { body = blockBody,
            parameterCount = blockParameterCount,
            localCount = blockLocalCount
          }

  selfIx <- getSelf

  locals <- Arr.fromList <$> createLocals selfIx blockParameterCount blockLocalCount

  let cf =
        BlockCallFrame
          { method = method,
            pc = 0,
            locals,
            capturedFrame = blockCapturedFrame
          }

  pushCallFrame cf

restart :: (CallStackEff r, Member ExcT r, SetMember Lift (Lift IO) r) => Eff r ()
restart = do
  popCallFrame >>= flip modifyCallFrame (\cf -> pure cf {pc = 0}) >>= modify @CallStackNat . St.push
