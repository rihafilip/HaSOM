module HaSOM.Parser.ParseTree where

import Data.List.NonEmpty (NonEmpty)

data Identifier = Primitive | NamedIdentifier String
  deriving (Eq, Show)

type Variable = Identifier

data Class = MkClass
  { name :: String,
    superclass :: Maybe String,
    instanceFields :: [Variable],
    instanceMethods :: [Method],
    classFields :: [Variable],
    classMethods :: [Method]
  }
  deriving (Eq, Show)

-----------------------------------------

data Method = MkMethod
  { methodType :: MethodType,
    methodBody :: MethodBody
  }
  deriving (Eq, Show)

type UnarySelector = Identifier
type BinarySelector = String
type Keyword = String

data MethodType
  = UnaryMethod UnarySelector
  | BinaryMethod BinarySelector Variable
  | KeywordMethod (NonEmpty (Keyword, Variable))
  deriving (Eq, Show)

data MethodBody
  = MethodPrimitive
  | MethodBlock (Maybe Block)
  deriving (Eq, Show)

data Block = MkBlock
  { localDefs :: [Variable]
  , blockBody :: Statement
  } deriving (Eq, Show)

-----------------------------------------

data Statement
  = St Expression (Maybe Statement)
  | Exit Expression
  deriving (Eq, Show)

data Expression
  = Assignation (NonEmpty Variable) Evaluation
  | Eval Evaluation
  deriving (Eq, Show)

data Evaluation
  = MkEvaluation Primary [UnarySelector] [BinaryMessage] [KeywordMessage]
  deriving (Eq, Show)

data Primary
  = PrimVar Variable
  | PrimTerm Expression
  | PrimBlock NestedBlock
  | PrimLit Literal
  deriving (Eq, Show)

-----------------------------------------

data BinaryMessage
  = MkBinaryMessage BinarySelector BinaryOperand
  deriving (Eq, Show)

data BinaryOperand
  = MkBinaryOperand Primary [UnarySelector]
  deriving (Eq, Show)

data KeywordMessage
  = MkKeywordMessage Keyword BinaryOperand [BinaryMessage]
  deriving (Eq, Show)

-----------------------------------------

data Literal
  = LArray [Literal]
  | LSymbol Symbol
  | LString String
  | LNumber Number
  deriving (Eq, Show)

data Symbol
  = Symbol String
  | UnSelector UnarySelector
  | BinSelector BinarySelector
  | KeywSelector (NonEmpty Keyword)
  deriving (Eq, Show)

data Number
  = NInteger Int
  | NDouble Double
  | NMinus Number
  deriving (Eq, Show)

-----------------------------------------

data NestedBlock = MkNestedBlock
  { blockArguments :: [Variable]
  , blockContents :: Maybe Block
  }
  deriving(Eq, Show)
