module HaSOM.VM.Primitive.Class (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

-- TODO
primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Class",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("name", mkNativeFun nameC),
    ("new", mkNativeFun new),
    ("superclass", mkNativeFun superclass),
    ("fields", mkNativeFun fields),
    ("methods", mkNativeFun methods)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance

nameC = undefined
new = undefined
superclass = undefined
fields = undefined
methods = undefined
