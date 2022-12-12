module HaSOM.Parser.ParseTree where

import HaSOM.AST (Variable, UnarySelector, BinarySelector, Keyword, Symbol)
import qualified HaSOM.AST as AST
import Data.List.NonEmpty (NonEmpty)

data Block = MkBlock
  { localDefs :: [Variable]
  , blockBody :: BlockBody
  } deriving (Eq, Show)

-----------------------------------------

data BlockBody
  = BlockBody Expression (Maybe BlockBody)
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

-----------------------------------------

transformBlock :: Maybe Block -> AST.Block
transformBlock = undefined
