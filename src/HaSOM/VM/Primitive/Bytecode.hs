-- | VM Bytecode definition
module HaSOM.VM.Primitive.Bytecode(Bytecode(..), Code) where
import HaSOM.VM.Primitive.VMArray (VMArray)
import HaSOM.VM.Primitive.Ix

-- | Representation of SOM Bytecode
data Bytecode
  = HALT
  | DUP
  | POP
  | PUSH_LITERAL LiteralIx
  | PUSH_LOCAL LocalIx LocalIx
  | PUSH_FIELD FieldIx
  | PUSH_GLOBAL GlobalIx
  | SET_LOCAL LocalIx LocalIx
  | SET_FIELD FieldIx
  | SET_GLOBAL GlobalIx
  | CALL LiteralIx
  | SUPER_CALL LiteralIx
  | RETURN
  | NONLOCAL_RETURN
  deriving (Eq, Show)

type Code = VMArray InsIx Bytecode
