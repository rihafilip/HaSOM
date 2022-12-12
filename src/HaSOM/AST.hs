module HaSOM.AST where

import Data.List.NonEmpty ( NonEmpty )

data Identifier = Primitive | NamedIdentifier String
  deriving (Eq, Show)

type Variable = Identifier

-------------------------------

type UnarySelector = Identifier
type BinarySelector = String
type Keyword = String

-------------------------------

data Class = MkClass
  { name :: String,
    superclass :: Maybe String,
    instanceFields :: [Variable],
    instanceMethods :: [Method],
    classFields :: [Variable],
    classMethods :: [Method]
  }
  deriving (Eq, Show)

------------------------------

data Method = MkMethod MethodType MethodBody
  deriving (Eq, Show)

data MethodType
  = UnaryMethod UnarySelector
  | BinaryMethod BinarySelector Variable
  | KeywordMethod (NonEmpty (Keyword, Variable))
  deriving (Eq, Show)

data MethodBody
  = MethodPrimitive
  | MethodBlock Block
  deriving (Eq, Show)

------------------------------

data Block = MkBlock [Variable] [Expression]
  deriving (Eq, Show)

-- primary unaryCall + secondOperand kw1: a kw2: b
-- ((primary unaryCall) + secondOperand) kw1: a kw2: b

data Expression
  = Exit Expression
  | Assignation (NonEmpty Variable) Expression
  | EvalExpression Expression

  | UnaryCall Expression UnarySelector
  | BinaryCall Expression BinarySelector Expression
  | KeywordCall Expression [KeywordMessage]

  | PrimaryVariable Variable
  | PrimaryBlock NestedBlock
  | PrimaryLiteral Literal
  deriving (Eq, Show)

data KeywordMessage = MkKeywordMessage Keyword Expression
  deriving (Eq, Show)

-- Arguments, Block
data NestedBlock = MkNestedBlock [Variable] Block
  deriving (Eq, Show)

------------------------------

data Literal
  = LArray [Literal]
  | LSymbol Symbol
  | LString String
  | LInteger Int
  | LDouble Double
  deriving (Eq, Show)

data Symbol
  = Symbol String
  | SUnSelector UnarySelector
  | SBinSelector BinarySelector
  | SKWSelector (NonEmpty Keyword)
  deriving (Eq, Show)
