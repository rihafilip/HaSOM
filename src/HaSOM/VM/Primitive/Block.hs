{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.Block (primitives, value) where

import Control.Eff.IO.Utility (lreadIORef)
import Control.Eff.Utility (modifyEff)
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
  [ ("value", value "Block>>#value"),
    ("restart", restart)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

value :: Text -> NativeFun
value signature = mkNativeFun $ do
  blockIx <- getSelf

  -- Pop the current callframe
  prevCallFrame <- popCallFrame

  (blockCapturedFrame, MkVMBlock {blockBody, blockLocalCount, blockParameterCount}) <-
    getAsObject blockIx >>= \case
      BlockObject {blockCapturedFrame, block} -> pure (blockCapturedFrame, block)
      obj -> wrongObjectType obj BlockT

  let method =
        BytecodeMethod
          { body = blockBody,
            parameterCount = blockParameterCount,
            localCount = blockLocalCount,
            signature = signature
          }

  selfIx <-
    lreadIORef blockCapturedFrame
      >>= throwOnNothing "No local in block construction" . Arr.get 0 . locals

  nilIx <- getNil
  let placeholderLocals = Arr.fromList (replicate blockLocalCount nilIx)
  locals <-
    getCallFrame prevCallFrame
      >>= throwOnNothing "An empty array" -- should not happen
        . Arr.set 0 selfIx -- set the 'self' to the holder object
        . (`Arr.append` placeholderLocals) -- push back place for locals
        . locals -- get current locals with
  methodHolder <- clazz <$> getAsObject selfIx
  callStackHeight <- St.size <$> get @CallStackNat

  let cf =
        BlockCallFrame
          { methodHolder,
            method = method,
            pc = 0,
            locals,
            callStackHeight,
            capturedFrame = blockCapturedFrame
          }

  -- Push the new call frame
  pushCallFrame cf
  pure Nothing

restart :: NativeFun
restart = mkNativeFun $ do
  _ <- popCallFrame -- Block>>restart call frame
  callIt <- popCallFrame
  csHeigth <- callStackHeight <$> getCallFrame callIt

  modifyCallFrame callIt (\cf -> pure cf {pc = 0})
    >>= modify @CallStackNat . St.push

  modifyEff @ObjStack $ \st ->
    throwOnNothing
      "Reseting too small of a stack in Block>>restart"
      $ St.popn (St.size st - csHeigth) st
  pure Nothing
