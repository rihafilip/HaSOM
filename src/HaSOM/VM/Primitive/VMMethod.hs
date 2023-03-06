-- | VM Method definition
module HaSOM.VM.Primitive.VMMethod
  ( -- * Method definition
    Method (..),

    -- * Methods collection definition
    Methods,
    newMethods,
    getMethod,
  )
where

import qualified Data.Map.Strict as Map
import GHC.Base (Nat)
import HaSOM.VM.Primitive.Ix (InsIx, SymbolIx)

-- TODO Built-In
-- | VM representation of SOM method
data Method = MkVMMethod
  { -- | Start index in class code
    codeIdx :: InsIx,
    parameterCount :: Nat,
    localsCount :: Nat
  }

-- | Collection of methods
newtype Methods = MkMethods (Map.Map SymbolIx Method)

-- | Create new collection of methods
newMethods :: [(SymbolIx, Method)] -> Methods
newMethods = MkMethods . Map.fromList

-- | Get a method from collection of methods
getMethod :: Methods -> SymbolIx -> Maybe Method
getMethod (MkMethods ms) = (`Map.lookup` ms)
