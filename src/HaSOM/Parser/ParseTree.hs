-- | Representation of Happy parser output
module HaSOM.Parser.ParseTree
  ( -- * Parser tree representation
    Block (..),
    BlockBody (..),
    Expression (..),
    Evaluation (..),
    Primary (..),
    BinaryMessage (..),
    BinaryOperand (..),
    KeywordMessage (..),
    KeywordFormula (..),
    Formula (..),
    Literal (..),
    Number (..),
    NestedBlock (..),

    -- * Transformation function
    transformBlock,
  )
where

import qualified Data.Bifunctor as Bf
import Data.List.NonEmpty (NonEmpty)
import HaSOM.AST (BinarySelector, Keyword, UnarySelector, Variable)
import qualified HaSOM.AST as AST
import qualified Data.Text as T

data Block = MkBlock
  { localDefs :: [Variable],
    blockBody :: BlockBody
  }
  deriving (Eq, Show)

-----------------------------------------

data BlockBody
  = BlockBody Expression (Maybe BlockBody)
  | Exit Expression
  | EmptyBlockBody
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

newtype KeywordMessage
  = MkKeywordMessage (NonEmpty KeywordFormula)
  deriving (Eq, Show)

data KeywordFormula
  = MkKeywordFormula Keyword Formula
  deriving (Eq, Show)

data Formula
  = MkFormula BinaryOperand [BinaryMessage]
  deriving (Eq, Show)

-----------------------------------------

data Literal
  = LArray [Literal]
  | LSymbol T.Text
  | LString T.Text
  | LNumber Number
  deriving (Eq, Show)

data Number
  = NInteger Int
  | NDouble Double
  | NMinus Number
  deriving (Eq, Show)

-----------------------------------------

data NestedBlock = MkNestedBlock
  { blockArguments :: [Variable],
    blockContents :: Maybe Block
  }
  deriving (Eq, Show)

-----------------------------------------

-- | Transform a ParseTree Block into AST Block
transformBlock :: Maybe Block -> AST.Block
transformBlock Nothing = AST.MkBlock [] []
transformBlock (Just (MkBlock vars body)) = AST.MkBlock vars (transformBlockBody body)

transformBlockBody :: BlockBody -> [AST.Expression]
transformBlockBody (BlockBody expr Nothing) = [transformExpression expr]
transformBlockBody (BlockBody expr (Just body)) = transformExpression expr : transformBlockBody body
transformBlockBody (Exit expr) = [AST.Exit $ transformExpression expr]
transformBlockBody EmptyBlockBody = []

transformExpression :: Expression -> AST.Expression
transformExpression (Assignation vars eval) = AST.Assignation vars $ transformEvaluation eval
transformExpression (Eval eval) = transformEvaluation eval

transformEvaluation :: Evaluation -> AST.Expression
transformEvaluation (MkEvaluation prim uns bins kws) =
  let primaryExpr = transformPrimary prim
      unaryExpr = foldUnarySelector primaryExpr uns
      binaryExpr = foldBinaryMessage unaryExpr bins
      keywordExpr = transformKeywordCall binaryExpr kws
   in keywordExpr

foldUnarySelector :: AST.Expression -> [UnarySelector] -> AST.Expression
foldUnarySelector = foldl AST.UnaryCall

foldBinaryMessage :: AST.Expression -> [BinaryMessage] -> AST.Expression
foldBinaryMessage = foldl (\prim (MkBinaryMessage selec operand) -> AST.BinaryCall prim selec (transformBinaryOperand operand))

transformPrimary :: Primary -> AST.Expression
transformPrimary (PrimVar var) = AST.PrimaryVariable var
transformPrimary (PrimTerm expr) = transformExpression expr
transformPrimary (PrimBlock block) = AST.PrimaryBlock $ transformNestedBlock block
transformPrimary (PrimLit lit) = AST.PrimaryLiteral $ transformLiteral lit

transformBinaryOperand :: BinaryOperand -> AST.Expression
transformBinaryOperand (MkBinaryOperand prim uns) = foldUnarySelector (transformPrimary prim) uns

transformKeywordMessage :: KeywordMessage -> NonEmpty AST.KeywordMessage
transformKeywordMessage (MkKeywordMessage formulas) = fmap transformKeywordFormula formulas

transformKeywordFormula :: KeywordFormula -> AST.KeywordMessage
transformKeywordFormula (MkKeywordFormula kw formula) =
  let formulaExpr = transformFormula formula
   in AST.MkKeywordMessage kw formulaExpr

transformFormula :: Formula -> AST.Expression
transformFormula (MkFormula binOp binMessages) =
  let binOpExpr = transformBinaryOperand binOp
   in foldBinaryMessage binOpExpr binMessages

transformKeywordCall :: AST.Expression -> [KeywordMessage] -> AST.Expression
transformKeywordCall = foldl f
  where
    f currPrimary kwMessage = AST.KeywordCall currPrimary (transformKeywordMessage kwMessage)

transformLiteral :: Literal -> AST.Literal
transformLiteral (LArray arr) = AST.LArray $ map transformLiteral arr
transformLiteral (LSymbol sym) = AST.LSymbol sym
transformLiteral (LString str) = AST.LString str
transformLiteral (LNumber num) =
  either AST.LInteger AST.LDouble $
    transformNumber num

transformNumber :: Number -> Either Int Double
transformNumber (NInteger n) = Left n
transformNumber (NDouble n) = Right n
transformNumber (NMinus n) = Bf.bimap negate negate $ transformNumber n

transformNestedBlock :: NestedBlock -> AST.NestedBlock
transformNestedBlock (MkNestedBlock args content) = AST.MkNestedBlock args block
  where
    block = transformBlock content
