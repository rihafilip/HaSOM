-- | VM Literal definiton
module HaSOM.VM.Object.VMLiteral (VMLiteral (..)) where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Text (Text)
import HaSOM.VM.Object.VMBlock (VMBlock)
import Data.Text.Utility ((<+))

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

