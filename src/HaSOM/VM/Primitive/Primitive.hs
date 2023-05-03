{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module HaSOM.VM.Primitive.Primitive (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import HaSOM.VM.Object
import Data.Text.Utility

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Primitive",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("signature", mkNativeFun sign),
    ("holder", mkNativeFun holderM),
    ("invokeOn:with:", mkNativeFun invokeOnWith)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

sign :: (UniverseEff r, Lifted IO r) => Eff r ()
sign = pureNativeFun @N0 $ \self Nil -> do
  signature <-
    getAsObject self >>= \case
      PrimitiveObject {methodValue} -> pure methodValue
      obj -> wrongObjectType obj PrimitiveT

  sym <- getLiteralE signature >>= \case
    SymbolLiteral lit -> pure lit
    lit -> throwT $ "Expected Symbol literal, got " <+ showT lit

  newSymbol sym >>= addToGC

holderM :: (UniverseEff r, Lifted IO r) => Eff r ()
holderM = pureNativeFun @N0 $ \self Nil -> do
  holderIx <-
    getAsObject self >>= \case
      PrimitiveObject {holder} -> pure holder
      obj -> wrongObjectType obj PrimitiveT

  asObject <$> getClass holderIx

invokeOnWith :: (UniverseEff r, Lifted IO r) => Eff r ()
invokeOnWith = nativeFun @N2 $ \self (primary :+: args :+: Nil) -> do
  (holderIx, methodValue) <-
    getAsObject self >>= \case
      PrimitiveObject {holder, methodValue} -> pure (holder, methodValue)
      obj -> wrongObjectType obj PrimitiveT

  (argsArr, _) <- castArray args

  sendMessage primary methodValue holderIx argsArr
