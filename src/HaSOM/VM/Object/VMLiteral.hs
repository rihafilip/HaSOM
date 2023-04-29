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
import Data.Text (Text, unpack)
import Data.Text.Utility ((<+))
import HaSOM.VM.Object.Ix
import HaSOM.VM.Object.VMBlock (VMBlock)

-- | Representation of SOM literal
data VMLiteral
  = IntLiteral Int
  | DoubleLiteral Double
  | StringLiteral Text
  | SymbolLiteral Text
  | BlockLiteral VMBlock

instance Hashable VMLiteral where
  hashWithSalt i (IntLiteral v) = hashWithSalt i v
  hashWithSalt i (DoubleLiteral v) = hashWithSalt i v
  hashWithSalt i (StringLiteral v) = hashWithSalt i v
  hashWithSalt i (SymbolLiteral v) = hashWithSalt i ("#" <+ v)
  hashWithSalt i (BlockLiteral _) = hashWithSalt i ("#block" :: String)

instance Eq VMLiteral where
  (IntLiteral a) == (IntLiteral b) = a == b
  (DoubleLiteral a) == (DoubleLiteral b) = a == b
  (StringLiteral a) == (StringLiteral b) = a == b
  (SymbolLiteral a) == (SymbolLiteral b) = a == b
  _ == _ = False

instance Show VMLiteral where
  show (IntLiteral v) = show v
  show (DoubleLiteral v) = show v
  show (StringLiteral v) = unpack v
  show (SymbolLiteral v) = "#" ++ unpack v
  show (BlockLiteral _) = "<block>"

-- | Representation of all literals
newtype VMLiterals = MkVMLiterals {runVMLiterals :: Map.HashMap LiteralIx VMLiteral}

-- | Create new literals from list
newLiterals :: [(LiteralIx, VMLiteral)] -> VMLiterals
newLiterals = MkVMLiterals . Map.fromList

-- | Get literal at index
getLiteral :: LiteralIx -> VMLiterals -> Maybe VMLiteral
getLiteral idx = Map.lookup idx . runVMLiterals

internLiteral :: VMLiteral -> VMLiterals -> (VMLiterals, LiteralIx)
internLiteral = undefined -- TODO

literalsToList :: VMLiterals -> [(LiteralIx, Text, VMLiteral)]
literalsToList = undefined . Map.toList . runVMLiterals -- TODO
