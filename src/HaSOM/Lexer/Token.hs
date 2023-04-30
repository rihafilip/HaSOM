-- | Definition of lexer tokens
module HaSOM.Lexer.Token(Token(..), tokenToText) where

import Data.Text (Text)
import Data.Text.Utility

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

-- | Pretty print a token to Text
tokenToText :: Token -> Text
tokenToText = \case
  Identifier txt -> "Identifier " <+ txt
  OperatorSequence txt -> "OperatorSequence " <+ txt
  Keyword txt -> "Keyword " <+ txt
  KeywordSequence txt -> "KeywordSequence " <+ txt
  STString txt -> "STString " <+ txt
  tk -> showT tk
