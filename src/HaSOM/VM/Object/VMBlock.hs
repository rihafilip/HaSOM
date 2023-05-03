-- | VM Block definiton
module HaSOM.VM.Object.VMBlock (VMBlock (..)) where

import HaSOM.Bytecode (Code)

-- | Representation of SOM Block,
-- parametrised by native function type
data VMBlock = MkVMBlock
  { blockBody :: Code,
    blockParameterCount :: Int,
    blockLocalCount :: Int
  }
