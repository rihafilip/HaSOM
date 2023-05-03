{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module HaSOM.VM.Primitive.Symbol (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Symbol",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("asString", asString)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

asString :: NativeFun
asString = pureNativeFun @N0 $ \self Nil -> do
  symbol <- castSymbol self
  newString symbol >>= addToGC
