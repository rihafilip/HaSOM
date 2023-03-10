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
import GHC.Base (Nat)
import HaSOM.VM.Primitive.Ix (SymbolIx)
import HaSOM.VM.Primitive.VMArray (VMArray)
import HaSOM.Bytecode ( Bytecode )

-- | VM representation of SOM method,
-- polymorphic on native method type
data VMMethod f
  = -- | Method represented in bytecode
    BytecodeMethod
      { body :: VMArray Bytecode,
        parameterCount :: Nat,
        localsCount :: Nat
      }
  | -- | Method represented by Haskell function
    NativeMethod f

-- | Collection of methods
newtype VMMethods f = MkVMMethods (Map.Map SymbolIx (VMMethod f))

-- | Create new collection of methods
newMethods :: [(SymbolIx, VMMethod f)] -> VMMethods f
newMethods = MkVMMethods . Map.fromList

-- | Get a method from collection of methods
getMethod :: VMMethods f -> SymbolIx -> Maybe (VMMethod f)
getMethod (MkVMMethods ms) = (`Map.lookup` ms)
