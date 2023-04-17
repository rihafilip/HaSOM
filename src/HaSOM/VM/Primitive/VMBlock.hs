module HaSOM.VM.Primitive.VMBlock (VMBlock (..)) where

import HaSOM.VM.Primitive.Bytecode (Code)

-- | Representation of SOM Block,
-- parametrised by native function type
data VMBlock = MkVMBlock
  { -- blockFieldsCount :: Int,
    -- blockArgumentCount :: Int,
    blockBody :: Code
  }
