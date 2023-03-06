-- | VM Symbol definiton
module HaSOM.VM.Primitive.VMSymbol(VMSymbol(..)) where
import Data.Text (Text)

-- | Representation of SOM symbol
data VMSymbol =
  IntSymbol Int
  | DoubleSymbol Double
  | StringSymbol Text
