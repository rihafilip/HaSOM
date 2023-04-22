module HaSOM.VM.Primitive.Method (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

-- TODO
primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Method",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("signature", mkNativeFun signature),
    ("holder", mkNativeFun holder),
    ("invokeOn:with:", mkNativeFun invokeOnWith)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

signature = undefined
holder = undefined
invokeOnWith = undefined
