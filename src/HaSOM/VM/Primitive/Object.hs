module HaSOM.VM.Primitive.Object (primitives) where

import Data.Text (Text)
import HaSOM.VM.Primitive.Type
import HaSOM.VM.Universe
import HaSOM.VM.Universe.Operations

-- TODO
primitives :: PrimitiveContainer
primitives =
  MkPrimitiveContainer
    { name = "Object",
      instanceMethods = instanceMs,
      classMethods = classMs
    }

instanceMs :: [(Text, NativeFun)]
instanceMs =
  [ ("class", mkNativeFun undefined),
    ("objectSize", mkNativeFun undefined),
    ("==", mkNativeFun undefined),
    ("hashcode", mkNativeFun undefined),
    ("inspect", mkNativeFun undefined),
    ("halt", mkNativeFun undefined),
    ("perform:", mkNativeFun undefined),
    ("perform:withArguments:", mkNativeFun undefined),
    ("perform:inSuperclass:", mkNativeFun undefined),
    ("perform:withArguments:inSuperclass:", mkNativeFun undefined),
    ("instVarAt:", mkNativeFun undefined),
    ("instVarAt:put:", mkNativeFun undefined),
    ("instVarNamed:", mkNativeFun undefined)
  ]

classMs :: [(Text, NativeFun)]
classMs = []

---------------------------------
-- Instance
