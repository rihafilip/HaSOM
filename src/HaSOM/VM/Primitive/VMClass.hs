module HaSOM.VM.Primitive.VMClass (VMClass (..), CoreClasses (..)) where

import HaSOM.VM.Primitive.Ix (GlobalIx, ObjIx)
import HaSOM.VM.Primitive.VMMethod (VMMethods)

-- | Representation of SOM Class,
-- parametrised by native function type
data VMClass f = MkVMClass
  { fieldsCount :: Int,
    superclass :: Maybe GlobalIx,
    asObject :: ObjIx,
    methods :: VMMethods f
  }

data CoreClasses = MkCoreClasses
  { -- meta objects
    classClass :: GlobalIx,
    metaclassClass :: GlobalIx,
    objectClass :: GlobalIx,
    -- system
    systemClass :: GlobalIx,
    -- invokables
    methodClass :: GlobalIx,
    primitiveClass :: GlobalIx,
    -- primitives
    booleanClass :: GlobalIx,
    integerClass :: GlobalIx,
    doubleClass :: GlobalIx,
    stringClass :: GlobalIx,
    symbolClass :: GlobalIx,
    arrayClass :: GlobalIx,
    -- nilClass :: GlobalIx,
    -- trueClass :: GlobalIx,
    -- falseClass :: GlobalIx,
    -- block
    blockClass :: GlobalIx,
    block1Class :: GlobalIx,
    block2Class :: GlobalIx,
    block3Class :: GlobalIx
  }
