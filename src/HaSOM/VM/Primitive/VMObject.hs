-- | VM Object defintion
module HaSOM.VM.Primitive.VMObject (VMObject (..)) where

import HaSOM.VM.Primitive.Ix
import HaSOM.VM.Primitive.VMArray

-- | Representation of SOM object
data VMObject = MkVMObject
  { classType :: ClassIx,
    fields :: VMArray ObjIx
  }
