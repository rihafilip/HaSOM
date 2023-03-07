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
import HaSOM.VM.Primitive.Ix (InsIx, SymbolIx)

-- TODO Built-In
-- | VM representation of SOM method
data VMMethod = MkVMMethod
  { -- | Start index in class code
    codeIdx :: InsIx,
    parameterCount :: Nat,
    localsCount :: Nat
  }

-- | Collection of methods
newtype VMMethods = MkVMMethods (Map.Map SymbolIx VMMethod)

-- | Create new collection of methods
newMethods :: [(SymbolIx, VMMethod)] -> VMMethods
newMethods = MkVMMethods . Map.fromList

-- | Get a method from collection of methods
getMethod :: VMMethods -> SymbolIx -> Maybe VMMethod
getMethod (MkVMMethods ms) = (`Map.lookup` ms)
