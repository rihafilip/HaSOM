module HaSOM.VM.Primitive.Primitive (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

-- TODO
primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Primitive",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("signature", mkNativeFun undefined),
    ("holder", mkNativeFun undefined),
    ("invokeOn:with:", mkNativeFun undefined)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance
