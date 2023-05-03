{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | VM Method definition
module HaSOM.VM.Object.VMMethod
  ( -- * Method definition
    VMMethod (..),

    -- * Methods collection definition
    VMMethods,
    newMethods,
    getMethod,

    -- * Transformation
    methodsAsList,
  )
where

import qualified Data.HashMap.Strict as Map
import HaSOM.VM.Object.Bytecode (Code)
import HaSOM.VM.Object.Ix (LiteralIx)
import Data.Text (Text)

-- | VM representation of SOM method,
-- polymorphic on native method type
data VMMethod f
  = -- | Method represented in bytecode
    BytecodeMethod
      { signature :: Text,
        body :: Code,
        parameterCount :: Int,
        localCount :: Int
      }
  | -- | Method represented by Haskell function
    NativeMethod
      { signature :: Text,
        nativeBody :: f,
        parameterCount :: Int
      }

-- | Collection of methods
newtype VMMethods f = MkVMMethods (Map.HashMap LiteralIx (VMMethod f))

-- | Create new collection of methods
newMethods :: [(LiteralIx, VMMethod f)] -> VMMethods f
newMethods = MkVMMethods . Map.fromList

-- | Get a method from collection of methods
getMethod :: LiteralIx -> VMMethods f -> Maybe (VMMethod f)
getMethod idx (MkVMMethods ms) = Map.lookup idx ms

methodsAsList :: VMMethods f -> [(LiteralIx, VMMethod f)]
methodsAsList (MkVMMethods ms) = Map.toList ms
