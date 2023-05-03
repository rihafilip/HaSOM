{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module HaSOM.VM.Primitive.Method (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import HaSOM.VM.Object.VMObject (VMObject(..))
import HaSOM.VM.Object (VMLiteral(..), VMClass (asObject))
import Data.Text.Utility

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Method",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("signature", sign),
    ("holder", holderM),
    ("invokeOn:with:", invokeOnWith)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

sign :: NativeFun
sign = pureNativeFun @N0 $ \self Nil -> do
  signature <-
    getAsObject self >>= \case
      MethodObject {methodValue} -> pure methodValue
      obj -> wrongObjectType obj MethodT

  sym <- getLiteralE signature >>= \case
    SymbolLiteral lit -> pure lit
    lit -> throwT $ "Expected Symbol literal, got " <+ showT lit

  newSymbol sym >>= addToGC

holderM :: NativeFun
holderM = pureNativeFun @N0 $ \self Nil -> do
  holderIx <-
    getAsObject self >>= \case
      MethodObject {holder} -> pure holder
      obj -> wrongObjectType obj MethodT

  asObject <$> getClass holderIx

invokeOnWith :: NativeFun
invokeOnWith = mkNativeFun $ do
  self <- getSelf
  primary <- getLocal 0 1
  args <- getLocal 0 2

  _ <- popCallFrame

  (holderIx, methodValue) <-
    getAsObject self >>= \case
      MethodObject {holder, methodValue} -> pure (holder, methodValue)
      obj -> wrongObjectType obj MethodT

  (argsArr, _) <- castArray args

  sendMessage primary methodValue holderIx argsArr
  pure Nothing
