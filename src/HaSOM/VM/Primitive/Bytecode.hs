-- | VM Bytecode definition
module HaSOM.VM.Primitive.Bytecode(Bytecode(..), Code) where
import HaSOM.VM.Primitive.VMArray (VMArray)
import HaSOM.VM.Primitive.Ix
import Data.Stack (StackIx)

-- | Representation of SOM Bytecode
data Bytecode
  = HALT
  | PUSH_SYMBOL SymbolIx
  | PUSH_LOCAL StackIx
  | PUSH_FIELD FieldIx
  | PUSH_GLOBAL GlobalIx
  | SET_LOCAL StackIx
  | SET_FIELD FieldIx
  | SET_GLOBAL GlobalIx
  | CALL
  | SUPER_CALL
  | RETURN
  | NONLOCAL_RETURN
  deriving (Eq, Show)

type Code = VMArray InsIx Bytecode
