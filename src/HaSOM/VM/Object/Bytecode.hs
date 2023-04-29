-- | VM Bytecode definition
module HaSOM.VM.Object.Bytecode(Bytecode(..), Code, code, getInstruction, codeAsList) where
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
getInstruction :: InsIx -> Code -> Maybe Bytecode
getInstruction idx = Arr.get idx . runCode

-- | Get the code as list
codeAsList :: Code -> [Bytecode]
codeAsList = Arr.toList . runCode
