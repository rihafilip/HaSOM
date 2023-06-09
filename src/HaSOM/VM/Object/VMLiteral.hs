-- | VM Literal definiton
module HaSOM.VM.Object.VMLiteral
  ( -- * Types definition
    VMLiteral (..),
    VMLiterals,

    -- * Literals manipulation
    newLiterals,
    getLiteral,

    -- * Interning helpers
    internLiteral,

    -- * Disassembl
    literalsToList,
  )
where

import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.LookupMap as LM
import Data.Text (Text, unpack)
import Data.Text.Utility ((<+))
import HaSOM.VM.Object.Ix
import HaSOM.VM.Object.VMBlock (VMBlock)
import Data.List (intercalate)

-- | Representation of SOM literal
data VMLiteral
  = IntLiteral Int
  | DoubleLiteral Double
  | StringLiteral Text
  | SymbolLiteral Text
  | ArrayLiteral [LiteralIx]
  | BlockLiteral VMBlock

instance Hashable VMLiteral where
  hashWithSalt i (IntLiteral v) = hashWithSalt i v
  hashWithSalt i (DoubleLiteral v) = hashWithSalt i v
  hashWithSalt i (StringLiteral v) = hashWithSalt i v
  hashWithSalt i (SymbolLiteral v) = hashWithSalt i ("#" <+ v)
  hashWithSalt i (ArrayLiteral vs) = hashWithSalt i vs
  hashWithSalt i (BlockLiteral _) = hashWithSalt i ("#block" :: String)

instance Eq VMLiteral where
  (IntLiteral a) == (IntLiteral b) = a == b
  (DoubleLiteral a) == (DoubleLiteral b) = a == b
  (StringLiteral a) == (StringLiteral b) = a == b
  (SymbolLiteral a) == (SymbolLiteral b) = a == b
  (ArrayLiteral a) == (ArrayLiteral b) = a == b
  _ == _ = False

instance Show VMLiteral where
  show (IntLiteral v) = show v
  show (DoubleLiteral v) = show v
  show (StringLiteral v) = unpack v
  show (SymbolLiteral v) = "#" ++ unpack v
  show (ArrayLiteral vs) = "#(" ++ intercalate ", " (map show vs) ++  ")"
  show (BlockLiteral _) = "<block>"

-- | Representation of all literals
data VMLiterals = MkVMLiterals
  { literals :: Map.HashMap LiteralIx VMLiteral,
    interner :: LM.LookupMap VMLiteral LiteralIx
  }

-- | Create new literals from list
newLiterals :: VMLiterals
newLiterals = MkVMLiterals Map.empty LM.new

-- | Get literal at index
getLiteral :: LiteralIx -> VMLiterals -> Maybe VMLiteral
getLiteral idx = Map.lookup idx . literals

-- | Get a literal index for given literal
internLiteral :: VMLiteral -> VMLiterals -> (VMLiterals, LiteralIx)
internLiteral lit MkVMLiterals {literals, interner} =
  ( MkVMLiterals
      { literals = lits,
        interner = inter
      },
    idx
  )
  where
    (inter, idx) = LM.getOrSet lit interner
    lits = Map.insert idx lit literals

literalsToList :: VMLiterals -> [(LiteralIx, VMLiteral)]
literalsToList = Map.toList . literals
