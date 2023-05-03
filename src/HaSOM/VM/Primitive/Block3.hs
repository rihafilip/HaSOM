module HaSOM.VM.Primitive.Block3 (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Block (value)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe

primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Block3",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("value:with:",  value "Block3>>value:with:")
  ]

classMs :: [(Text, NativeFun)]
classMs = []
