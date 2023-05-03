{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.VM.Primitive.Primitive (primitives) where

import Data.Text (Text)
import Data.Text.Utility
import HaSOM.VM.Object
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Primitive",
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
      PrimitiveObject {methodValue} -> pure methodValue
      obj -> wrongObjectType obj PrimitiveT

  sym <-
    getLiteralE signature >>= \case
      SymbolLiteral lit -> pure lit
      lit -> throwT $ "Expected Symbol literal, got " <+ showT lit

  newSymbol sym >>= addToGC

holderM :: NativeFun
holderM = pureNativeFun @N0 $ \self Nil -> do
  holderIx <-
    getAsObject self >>= \case
      PrimitiveObject {holder} -> pure holder
      obj -> wrongObjectType obj PrimitiveT

  asObject <$> getClass holderIx

invokeOnWith :: NativeFun
invokeOnWith = nativeFun @N2 $ \self (primary :+: args :+: Nil) -> do
  (holderIx, methodValue) <-
    getAsObject self >>= \case
      PrimitiveObject {holder, methodValue} -> pure (holder, methodValue)
      obj -> wrongObjectType obj PrimitiveT

  (argsArr, _) <- castArray args

  sendMessage primary methodValue holderIx argsArr
