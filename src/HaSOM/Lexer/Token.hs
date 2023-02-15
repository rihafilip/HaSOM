-- | Definition of lexer tokens
module HaSOM.Lexer.Token(Token(..)) where

-- | SOM tokens
data Token
  = -- | "primitive" string
    TPrimitive
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

  | OperatorSequence String

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
  | KeywordSequence String

  | STString String
  deriving (Eq, Show)
