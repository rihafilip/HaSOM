-- | VM Bytecode definition
module HaSOM.Bytecode(Bytecode(..)) where

-- | Representation of SOM Bytecode
data Bytecode
  = PUSH_CLASS Int
  | PUSH_SYMBOL Int
  | PUSH_LOCAL Int
  | PUSH_FIELD Int
  | SET_LOCAL Int
  | SET_FIELD Int
  | CALL Int
  | SUPER_CALL Int
  | RETURN
  | NONLOCAL_RETURN -- TODO
  deriving (Eq, Show)
