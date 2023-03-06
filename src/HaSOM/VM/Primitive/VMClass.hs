-- | VM Class definition
module HaSOM.VM.Primitive.VMClass(VMClass(..)) where

import HaSOM.Bytecode (Bytecode)
import HaSOM.VM.Primitive.VMMethod
import HaSOM.VM.Primitive.Ix
import HaSOM.VM.Primitive.VMArray

-- TODO builtin
-- | VM representation of SOM class
data VMClass = MkVMClass {
  -- | Index of superclass or Nothing if superclass is nil
  superclass :: Maybe ClassIx,
  methods :: Methods,
  -- | Representation of class as object
  -- (class methods and class fields)
  object :: ObjIx,
  -- | The executable bytecode
  code :: VMArray Bytecode
}
