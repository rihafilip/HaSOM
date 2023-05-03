{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | VM Object defintion
module HaSOM.VM.Object.VMObject
  ( -- * Object type
    VMObject (..),

    -- * Fields type
    Fields,
    newFields,
    setField,
    getField,
  )
where

import Data.IORef (IORef)
import Data.Text (Text)
import HaSOM.VM.Object.CallStack (CallFrame)
import HaSOM.VM.Object.Ix
import HaSOM.VM.Object.VMBlock
import HaSOM.VM.Object.VMClass
import qualified HaSOM.VM.VMArray as Arr

-------------------------------------------------------

-- | Fields in an objects defintion
newtype Fields = MkFields {runFields :: Arr.VMArray FieldIx ObjIx}

instance Show Fields where
  show = show . runFields

-- | Create new fields for a class with given nil object
newFields :: VMClass f -> ObjIx -> Fields
newFields MkVMClass {instanceFields} nil = MkFields $ Arr.new (Arr.length instanceFields) nil

-- | Set field in object
setField :: FieldIx -> ObjIx -> VMObject f -> Maybe (VMObject f)
setField fi oi obj =
  (\new -> obj {fields = new})
    . MkFields
    <$> Arr.set fi oi (runFields $ fields obj)

-- | Get field object in object
getField :: FieldIx -> VMObject f -> Maybe ObjIx
getField idx = Arr.get idx . runFields . fields

-------------------------------------------------------

-- | Representation of SOM object,
-- parametrised by native function type
data VMObject f
  = -- Regular instances
    InstanceObject
      { clazz :: VMClass f,
        fields :: Fields
      }
  | ClassObject
      { clazz :: VMClass f,
        fields :: Fields,
        classOf :: GlobalIx
      }
  | -- Primitives
    IntObject
      { clazz :: VMClass f,
        fields :: Fields,
        intValue :: Int
      }
  | DoubleObject
      { clazz :: VMClass f,
        fields :: Fields,
        doubleValue :: Double
      }
  | StringObject
      { clazz :: VMClass f,
        fields :: Fields,
        stringValue :: Text
      }
  | SymbolObject
      { clazz :: VMClass f,
        fields :: Fields,
        symbolValue :: Text
      }
  | ArrayObject
      { clazz :: VMClass f,
        fields :: Fields,
        arrayValue :: Arr.VMArray FieldIx ObjIx
      }
  | -- Invokables
    BlockObject
      { clazz :: VMClass f,
        fields :: Fields,
        blockCapturedFrame :: IORef (CallFrame f),
        block :: VMBlock
      }
  | MethodObject
      { clazz :: VMClass f,
        fields :: Fields,
        methodValue :: LiteralIx,
        holder :: GlobalIx
      }
  | PrimitiveObject
      { clazz :: VMClass f,
        fields :: Fields,
        methodValue :: LiteralIx,
        holder :: GlobalIx
      }
