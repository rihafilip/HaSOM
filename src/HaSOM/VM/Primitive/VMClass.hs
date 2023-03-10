-- | VM Class definition
module HaSOM.VM.Primitive.VMClass (VMClass (..)) where

import HaSOM.VM.Primitive.Ix
import HaSOM.VM.Primitive.VMMethod

-- | VM representation of SOM class
data VMClass f = MkVMClass
  { name :: SymbolIx,
    methods :: VMMethods f
  }
