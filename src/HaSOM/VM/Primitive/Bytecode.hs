-- | VM Bytecode definition
module HaSOM.VM.Primitive.Bytecode(Bytecode(..), Code) where
import HaSOM.VM.Primitive.VMArray (VMArray)
import HaSOM.VM.Primitive.Ix
import Data.Stack (StackIx)

-- | Representation of SOM Bytecode
data Bytecode
  = PUSH_CLASS ClassIx
  | PUSH_SYMBOL SymbolIx
  | PUSH_LOCAL StackIx
  | PUSH_FIELD FieldIx
  | SET_LOCAL StackIx
  | SET_FIELD FieldIx
  | CALL
  | SUPER_CALL
  | RETURN
  | NONLOCAL_RETURN -- TODO
  deriving (Eq, Show)

type Code = VMArray Bytecode
