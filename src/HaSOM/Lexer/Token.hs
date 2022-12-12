module HaSOM.Lexer.Token where

data Token
  = TPrimitive
  | Identifier String

  | Equal

  | Separator

  | NewTerm
  | EndTerm
  | Or

  | Comma
  | Minus
  | Not
  | And
  | Star
  | Div
  | Mod
  | Plus
  | More
  | Less
  | At
  | Per

  -- | OperatorSequence (NonEmpty Operator)

  | Colon

  | NewBlock
  | EndBlock

  | Pound
  | TExit
  | Period
  | Assign

  | Integer Int
  | Double Double

  | Keyword String
  -- | KeywordSequence (NonEmpty Keyword)

  | STString String
  deriving (Eq, Show)
