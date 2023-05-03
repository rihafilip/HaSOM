module HaSOM.VM.Primitive.Double (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

-- TODO
primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Double",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("at:", mkNativeFun undefined)
  ]

classMs :: [(Text, NativeFun)]
classMs =
  [ ("new", mkNativeFun undefined)
  ]

---------------------------------
-- Instance

---------------------------------
-- Class
