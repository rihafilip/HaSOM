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
import HaSOM.VM.Primitive.Ix (SymbolIx)
import HaSOM.VM.Primitive.Bytecode (Code)

-- | VM representation of SOM method,
-- polymorphic on native method type
data VMMethod f
  = -- | Method represented in bytecode
    BytecodeMethod
      { body :: Code,
        parameterCount :: Int
        -- localsCount :: Int
      }
  | -- | Method represented by Haskell function
    NativeMethod f

-- TODO as HashMap
-- | Collection of methods
newtype VMMethods f = MkVMMethods (Map.Map SymbolIx (VMMethod f))

-- | Create new collection of methods
newMethods :: [(SymbolIx, VMMethod f)] -> VMMethods f
newMethods = MkVMMethods . Map.fromList

-- | Get a method from collection of methods
getMethod :: VMMethods f -> SymbolIx -> Maybe (VMMethod f)
getMethod (MkVMMethods ms) = (`Map.lookup` ms)
