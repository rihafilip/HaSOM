{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | VM Object defintion
module HaSOM.VM.Primitive.VMObject
  ( -- * Type
    VMObject (..),

    -- * VMObject manipulation
    newInstance,
    setField,
    getField,
  )
where

import Combinator
import Data.Text (Text)
import HaSOM.VM.Primitive.Ix
import HaSOM.VM.Primitive.VMArray
import HaSOM.VM.Primitive.VMBlock
import HaSOM.VM.Primitive.VMClass

type FieldArray = VMArray FieldIx ObjIx

-- | Representation of SOM object,
-- parametrised by native function type
data VMObject f
  = InstanceObject
      { clazz :: VMClass f,
        fields :: FieldArray
      }
  | ClassObject
      { clazz :: VMClass f,
        fields :: FieldArray
      }
  | IntObject
      { clazz :: VMClass f,
        fields :: FieldArray,
        intValue :: Int
      }
  | DoubleObject
      { clazz :: VMClass f,
        fields :: FieldArray,
        doubleValue :: Double
      }
  | ArrayObject
      { clazz :: VMClass f,
        fields :: FieldArray,
        arrayValue :: VMArray FieldIx ObjIx
      }
  | BlockObject
      { clazz :: VMClass f,
        fields :: FieldArray,
        frameId :: Int,
        block :: VMBlock
      }
  | StringObject
      { clazz :: VMClass f,
        fields :: FieldArray,
        stringValue :: Text
      }
  | SymbolObject
      { clazz :: VMClass f,
        fields :: FieldArray,
        symbolValue :: Text
      }

-- | New InstanceObject of given class type
newInstance :: VMClass f -> ObjIx -> VMObject f
newInstance cl@MkVMClass {fieldsCount} nil = InstanceObject {clazz = cl, fields = newArray fieldsCount nil}

-- | Set field in object
setField :: VMObject f -> FieldIx -> ObjIx -> Maybe (VMObject f)
setField source =
  fmap (\newFields -> source {fields = newFields})
    ... setArray (fields source)

-- | Get field object in object
getField :: VMObject f -> FieldIx -> Maybe ObjIx
getField = getArray . fields
