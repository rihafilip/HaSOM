module HaSOM.VM.Object.VMBlock (VMBlock (..)) where

import HaSOM.VM.Object.Bytecode (Code)

-- | Representation of SOM Block,
-- parametrised by native function type
data VMBlock = MkVMBlock
  { blockBody :: Code,
    blockParameterCount :: Int,
    blockLocalCount :: Int
  }
