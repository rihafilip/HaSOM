{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | VM Method definition
module HaSOM.VM.Object.VMMethod
  ( -- * Method definition
    VMMethod (..),

    -- * Methods collection definition
    VMMethods,
    newMethods,
    getMethod,
  )
where

import qualified Data.Map.Strict as Map
import HaSOM.VM.Object.Ix (LiteralIx)
import HaSOM.VM.Object.Bytecode (Code)

-- | VM representation of SOM method,
-- polymorphic on native method type
data VMMethod f
  = -- | Method represented in bytecode
    BytecodeMethod
      { body :: Code,
        parameterCount :: Int,
        localCount :: Int
      }
  | -- | Method represented by Haskell function
    NativeMethod {
      nativeBody :: f,
      parameterCount :: Int
    }


-- TODO as HashMap
-- | Collection of methods
newtype VMMethods f = MkVMMethods (Map.Map LiteralIx (VMMethod f))

-- | Create new collection of methods
newMethods :: [(LiteralIx, VMMethod f)] -> VMMethods f
newMethods = MkVMMethods . Map.fromList

-- | Get a method from collection of methods
getMethod :: LiteralIx -> VMMethods f -> Maybe (VMMethod f)
getMethod idx (MkVMMethods ms) = Map.lookup idx ms
