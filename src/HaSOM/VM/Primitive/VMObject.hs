{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | VM Object defintion
module HaSOM.VM.Primitive.VMObject
  ( -- * Types
    VMClass (..),
    VMObject (..),

    -- * VMObject manipulation
    newInstance,
    setField,
    getField,
  )
where

import Combinator
import Data.Text (Text)
import HaSOM.VM.Primitive.Bytecode (Code)
import HaSOM.VM.Primitive.Ix
import HaSOM.VM.Primitive.VMArray
import HaSOM.VM.Primitive.VMMethod

-- | Representation of SOM Class,
-- parametrised by native funciton type
data VMClass f = MkVMClass
  { fieldsCount :: Int,
    superclass :: Maybe ClassIx,
    asObject :: VMObject f,
    methods :: VMMethods f
  }

-- | Representation of SOM object,
-- parametrised by native function type
data VMObject f
  = InstanceObject
      { clazz :: VMClass f,
        fields :: VMArray ObjIx
      }
  | IntObject
      { clazz :: VMClass f,
        fields :: VMArray ObjIx,
        intValue :: Int
      }
  | DoubleObject
      { clazz :: VMClass f,
        fields :: VMArray ObjIx,
        doubleValue :: Double
      }
  | ArrayObject
      { clazz :: VMClass f,
        fields :: VMArray ObjIx,
        arrayValue :: VMArray ObjIx
      }
  | BlockObject
      { clazz :: VMClass f,
        fields :: VMArray ObjIx,
        frame :: (), -- TODO
        blockFieldsCount :: Int,
        blockArgumentCount :: Int,
        blockBody :: Code
      }
  | StringObject
      { clazz :: VMClass f,
        fields :: VMArray ObjIx,
        stringValue :: Text
      }
  | SymbolObject
      { clazz :: VMClass f,
        fields :: VMArray ObjIx,
        symbolValue :: Text
      }

-- | New InstanceObject of given class type
newInstance :: VMClass f -> ObjIx -> VMObject f
newInstance cl@MkVMClass {fieldsCount} nil = InstanceObject {clazz = cl, fields = newArray fieldsCount nil}

-- | Set field in object
setField :: VMObject f -> ArrayIx -> ObjIx -> Maybe (VMObject f)
setField source =
  fmap (\newFields -> source {fields = newFields})
    ... setArray (fields source)

-- | Get field object in object
getField :: VMObject f -> ArrayIx -> Maybe ObjIx
getField = getArray . fields
