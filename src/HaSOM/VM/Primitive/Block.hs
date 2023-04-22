{-# LANGUAGE FlexibleContexts #-}
module HaSOM.VM.Primitive.Block (primitives, value) where

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

value :: (CallStackEff r, ObjStackEff r, Member ExcT r, GCEff r) => Eff r ()
value = do
  objIx <- popStack

  (capturedFrame, MkVMBlock {blockBody, blockLocalCount, blockParameterCount}) <-
    getAsObject objIx >>= \case
      BlockObject {capturedFrameId, block} -> pure (capturedFrameId, block)
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
            capturedFrame = undefined -- TODO
          }

  pushCallFrame cf

restart :: (CallStackEff r, Member ExcT r) => Eff r ()
restart = do
  cf <- popCallFrame
  let cf' = cf {pc = 0}
  -- TODO pop stack
  pushCallFrame cf'
