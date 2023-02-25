-- | Definition of lexer tokens
module HaSOM.Lexer.Token(Token(..)) where

import Data.Text (Text)

-- | SOM tokens
data Token
  = -- | "primitive" string
    TPrimitive
  | Identifier Text

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

  | OperatorSequence Text

  | Colon

  | NewBlock
  | EndBlock

  | Pound
  | TExit
  | Period
  | Assign

  | Integer Int
  | Double Double

  | Keyword Text
  | KeywordSequence Text

  | STString Text
  deriving (Eq, Show)
