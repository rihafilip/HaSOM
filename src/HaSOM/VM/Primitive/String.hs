module HaSOM.VM.Primitive.String (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

-- TODO
primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "String",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("concatenate:", mkNativeFun undefined),
    ("asSymbol", mkNativeFun undefined),
    ("hashcode", mkNativeFun undefined),
    ("length", mkNativeFun undefined),
    ("isWhiteSpace", mkNativeFun undefined),
    ("isLetters", mkNativeFun undefined),
    ("isDigits", mkNativeFun undefined),
    ("=", mkNativeFun undefined),
    ("primSubstringFrom:to:", mkNativeFun undefined)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance
