-- | VM Bytecode definition
module HaSOM.VM.Object.Bytecode(Bytecode(..), Code) where
import HaSOM.VM.Object.VMArray (VMArray)
import HaSOM.VM.Object.Ix

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
