module HaSOM.VM.Primitive.Block1 (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Block (value)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Block1",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("value", value "Block1>>#value")
  ]

classMs :: [(Text, NativeFun)]
classMs = []
