-- | VM Global definiton
module HaSOM.VM.Object.VMGlobal
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
import HaSOM.VM.Object.Ix (GlobalIx)
import HaSOM.VM.Object.VMClass (VMClass)
import HaSOM.VM.Object.VMObject (VMObject)

-- | Representation of global object,
-- parametrized by primitive function type
data VMGlobal f
  = ClassGlobal (VMClass f)
  | ObjectGlobal (VMObject f)

-- | Representation of all global objects,
-- parametrized by primitive function type
newtype VMGlobals f = MkVMGlobals {runGlobals :: Map.HashMap GlobalIx (VMGlobal f)}

-- | Create new globals from list
newGlobals :: [(GlobalIx, VMGlobal f)] -> VMGlobals f
newGlobals = MkVMGlobals . Map.fromList

-- | Get global at index
getGlobal :: GlobalIx -> VMGlobals f -> Maybe (VMGlobal f)
getGlobal idx = Map.lookup idx . runGlobals

-- | Set global at index
setGlobal :: GlobalIx -> VMGlobal f -> VMGlobals f -> VMGlobals f
setGlobal idx g = MkVMGlobals . Map.insert idx g . runGlobals
