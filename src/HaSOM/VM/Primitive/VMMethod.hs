{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | VM Method definition
module HaSOM.VM.Primitive.VMMethod
  ( -- * Method definition
    VMMethod (..),

    -- * Methods collection definition
    VMMethods,
    newMethods,
    getMethod,
  )
where

import qualified Data.Map.Strict as Map
import HaSOM.VM.Primitive.Ix (LiteralIx)
import HaSOM.VM.Primitive.Bytecode (Code)

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
    NativeMethod f

-- TODO as HashMap
-- | Collection of methods
newtype VMMethods f = MkVMMethods (Map.Map LiteralIx (VMMethod f))

-- | Create new collection of methods
newMethods :: [(LiteralIx, VMMethod f)] -> VMMethods f
newMethods = MkVMMethods . Map.fromList

-- | Get a method from collection of methods
getMethod :: VMMethods f -> LiteralIx -> Maybe (VMMethod f)
getMethod (MkVMMethods ms) = (`Map.lookup` ms)
