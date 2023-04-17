module HaSOM.VM.Primitive.VMClass(VMClass(..)) where

import HaSOM.VM.Primitive.Ix ( ObjIx )
import HaSOM.VM.Primitive.VMMethod ( VMMethods )

-- | Representation of SOM Class,
-- parametrised by native function type
data VMClass f = MkVMClass
  { fieldsCount :: Int,
    superclass :: Maybe (VMClass f),
    asObject :: ObjIx,
    methods :: VMMethods f
  }
