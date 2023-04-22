{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module HaSOM.VM.Primitive.System (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations
import HaSOM.VM.Object
import Combinator ((.>))

-- TODO
primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "System",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("global:", mkNativeFun global),
    ("global:put:", mkNativeFun globalPut),
    ("hasGlobal:", mkNativeFun hasGlobal),
    ("loadFile:", mkNativeFun undefined),
    ("load:", mkNativeFun undefined),
    ("resolve:", mkNativeFun undefined),
    ("exit:", mkNativeFun undefined),
    ("printString:", mkNativeFun undefined),
    ("printNewline", mkNativeFun undefined),
    ("errorPrintln:", mkNativeFun undefined),
    ("errorPrint:", mkNativeFun undefined),
    ("printStackTrace", mkNativeFun undefined),
    ("time", mkNativeFun undefined),
    ("ticks", mkNativeFun undefined),
    ("fullGC", mkNativeFun undefined)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

global :: UniverseEff r => Eff r ()
global = pureNativeFun @N1 $ \_ (g :+: Nil) -> do
  symbol <- castSymbol g

  idx <- undefined -- TODO

  get @VMGlobalsNat >>= getGlobal idx .> \case
    Nothing -> getNil
    Just (ObjectGlobal oi) -> pure oi
    Just (ClassGlobal MkVMClass{asObject}) -> pure asObject

globalPut :: UniverseEff r => Eff r ()
globalPut = pureNativeFun @N2 $ \self (g :+: val :+: Nil) -> do
  symbol <- castSymbol g

  idx <- undefined -- TODO

  setGlobalE idx (ObjectGlobal val)
  pure self

hasGlobal :: UniverseEff r => Eff r ()
hasGlobal = pureNativeFun @N1 $ \_ (s :+: Nil) -> do
  symbol <- castSymbol s

  idx <- undefined -- TODO

  bool <- get @VMGlobalsNat >>= getGlobal idx .> \case
    Just _ -> newTrue
    Nothing -> newFalse

  addToGC bool
