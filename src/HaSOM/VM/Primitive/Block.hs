{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

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
  [ ("value", value "Block>>value"),
    ("restart", restart)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

-- TODO pops automatically
value :: Text -> NativeFun
value signature = nativeFun @N0 $ \objIx Nil -> do
  (blockCapturedFrame, MkVMBlock {blockBody, blockLocalCount, blockParameterCount}) <-
    getAsObject objIx >>= \case
      BlockObject {blockCapturedFrame, block} -> pure (blockCapturedFrame, block)
      obj -> wrongObjectType obj BlockT

  let method =
        BytecodeMethod
          { body = blockBody,
            parameterCount = blockParameterCount,
            localCount = blockLocalCount,
            signature = signature
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

  _ <- popCallFrame
  pushCallFrame cf

-- TODO reset stack
restart :: NativeFun
restart = mkNativeFun $ do
  popCallFrame >>= flip modifyCallFrame (\cf -> pure cf {pc = 0}) >>= modify @CallStackNat . St.push
