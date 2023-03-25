-- | VM Symbol definiton
module HaSOM.VM.Primitive.VMSymbol (VMSymbol (..)) where

import Data.Hashable (Hashable (hashWithSalt))
import Data.Text (Text)

-- | Representation of SOM symbol
data VMSymbol
  = IntSymbol Int
  | DoubleSymbol Double
  | StringSymbol Text
  deriving (Eq)

instance Hashable VMSymbol where
  hashWithSalt i (IntSymbol v) = hashWithSalt i v
  hashWithSalt i (DoubleSymbol v) = hashWithSalt i v
  hashWithSalt i (StringSymbol v) = hashWithSalt i v
