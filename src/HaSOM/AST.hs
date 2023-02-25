-- | Representation of SOM class AST
module HaSOM.AST
  ( -- * Primitives
    Identifier,
    Variable,
    UnarySelector,
    BinarySelector,
    Keyword,

    -- * Class definitions
    Class (..),
    Method (..),
    MethodType (..),
    MethodBody (..),

    -- * Blocks and expressions
    Block (..),
    Expression (..),
    KeywordMessage (..),
    NestedBlock (..),
    Literal (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T

type Identifier = T.Text

-------------------------------
type Variable = Identifier

type UnarySelector = Identifier

type BinarySelector = Identifier

type Keyword = Identifier

-------------------------------

data Class = MkClass
  { name :: Identifier,
    superclass :: Maybe Identifier,
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

data Expression
  = Exit Expression
  | -- | Variable := ... Variable := Expression
    Assignation (NonEmpty Variable) Expression
  | UnaryCall Expression UnarySelector
  | BinaryCall Expression BinarySelector Expression
  | KeywordCall Expression (NonEmpty KeywordMessage)
  | PrimaryVariable Variable
  | PrimaryBlock NestedBlock
  | PrimaryLiteral Literal
  deriving (Eq, Show)

data KeywordMessage = MkKeywordMessage Keyword Expression
  deriving (Eq, Show)

-- | Arguments and the actual block
data NestedBlock = MkNestedBlock [Variable] Block
  deriving (Eq, Show)

------------------------------

data Literal
  = LArray [Literal]
  | LSymbol T.Text
  | LString T.Text
  | LInteger Int
  | LDouble Double
  deriving (Eq, Show)
