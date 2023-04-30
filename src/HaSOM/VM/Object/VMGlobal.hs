-- | VM Global definiton
module HaSOM.VM.Object.VMGlobal
  ( -- * Types defintion
    VMGlobal (..),
    VMGlobals,

    -- * Globals manipulation
    newGlobals,
    getGlobal,
    setGlobal,

    -- * Interning helpers
    internGlobal,
    getGlobalName,

    -- * Disassembly
    globalsToList,
  )
where

import qualified Data.HashMap.Strict as Map
import qualified Data.LookupMap as LM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import HaSOM.VM.Object.Ix (GlobalIx, ObjIx)
import HaSOM.VM.Object.VMClass (VMClass)

-- | Representation of global object,
-- parametrized by primitive function type
data VMGlobal f
  = ClassGlobal (VMClass f)
  | ObjectGlobal ObjIx

-- | Representation of all global objects,
-- parametrized by primitive function type
data VMGlobals f = MkVMGlobals
  { globals :: Map.HashMap GlobalIx (VMGlobal f),
    interner :: LM.LookupMap Text GlobalIx
  }

-- | Create new globals from list
newGlobals :: VMGlobals f
newGlobals = MkVMGlobals {globals = Map.empty, interner = LM.new}

-- | Get global at index
getGlobal :: GlobalIx -> VMGlobals f -> Maybe (VMGlobal f)
getGlobal idx = Map.lookup idx . globals

-- | Set global at index
setGlobal :: GlobalIx -> VMGlobal f -> VMGlobals f -> VMGlobals f
setGlobal idx g globs@MkVMGlobals {globals} =
  globs {globals = Map.insert idx g globals}

-- | Get a global index for given global object
internGlobal :: Text -> VMGlobals f -> (VMGlobals f, GlobalIx)
internGlobal txt gl@MkVMGlobals {interner} = (gl {interner = newInterner}, idx)
  where
    (newInterner, idx) = LM.getOrSet txt interner

-- | Get a name for given global object
getGlobalName :: GlobalIx -> VMGlobals f -> Maybe Text
getGlobalName idx = lookup idx . map swap . LM.toList . interner

globalsToList :: VMGlobals f -> [(GlobalIx, Text, VMGlobal f)]
globalsToList gl@MkVMGlobals {globals} = map f $ Map.toList globals
  where
    f (idx, val) = (idx, fromMaybe "" $ getGlobalName idx gl, val)
