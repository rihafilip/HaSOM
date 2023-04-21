-- | VM Bytecode definition
module HaSOM.VM.Object.Bytecode(Bytecode(..), Code, code, getBytecode) where
import HaSOM.VM.VMArray (VMArray)
import qualified HaSOM.VM.VMArray as Arr
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

-- | Representation of code block
newtype Code = MkCode { runCode :: VMArray InsIx Bytecode }

-- | Create a code block
code :: [Bytecode] -> Code
code = MkCode . Arr.fromList

-- | Get an instruction from code block
getBytecode :: InsIx -> Code -> Maybe Bytecode
getBytecode idx = Arr.get idx . runCode
