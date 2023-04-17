{-# OPTIONS_GHC -Wno-orphans #-}

module HaSOM.VM.Primitive.VMGlobal
  ( -- * Types defintion
    VMGlobal (..),
    VMGlobals (..),

    -- * Globals manipulation
    newGlobals,
    getGlobal,
    setGlobal,
  )
where

import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable (hashWithSalt))
import HaSOM.VM.Primitive.Ix (GlobalIx, getIx)
import HaSOM.VM.Primitive.VMClass (VMClass)
import HaSOM.VM.Primitive.VMObject (VMObject)

-- | Representation of global object,
-- parametrized by primitive function type
data VMGlobal f
  = ClassGlobal (VMClass f)
  | ObjectGlobal (VMObject f)

-- | Representation of all global objects,
-- parametrized by primitive function type
newtype VMGlobals f = MkVMGlobals {runGlobals :: Map.HashMap GlobalIx (VMGlobal f)}

instance Hashable GlobalIx where
  hashWithSalt = flip (hashWithSalt . getIx)

-- | Create new globals from list
newGlobals :: [(GlobalIx, VMGlobal f)] -> VMGlobals f
newGlobals = MkVMGlobals . Map.fromList

-- | Get global at index
getGlobal :: VMGlobals f -> GlobalIx -> Maybe (VMGlobal f)
getGlobal = flip Map.lookup . runGlobals

-- | Set global at index
setGlobal :: VMGlobals f -> GlobalIx -> VMGlobal f -> VMGlobals f
setGlobal (MkVMGlobals {runGlobals}) idx g = MkVMGlobals $ Map.insert idx g runGlobals
