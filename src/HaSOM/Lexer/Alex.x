{
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Alex generated lexer
module HaSOM.Lexer.Alex(alexScanTokens, PosToken(..), AlexPosn(..)) where

import HaSOM.Lexer.Token
import HaSOM.Lexer.Alex.Parsing
import Data.ByteString.Lazy (ByteString)

}

%wrapper "posn-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  \" (\n | ~\")* \" ;
  $white+ ;

  "primitive" { tok TPrimitive }
  $alpha [$alpha $digit _]* { tokT Identifier decode }

  "=" { tok Equal }

  "----" "-"* { tok Separator }

  "(" { tok NewTerm }
  ")" { tok EndTerm }
  "|" { tok Or }

  ","   { tok Comma }
  "-"   { tok Minus }
  "~"   { tok Not }
  "&"   { tok And }
  "*"   { tok Star }
  "/"   { tok Div }
  "\"   { tok Mod }
  "+"   { tok Plus }
  ">"   { tok More }
  "<"   { tok Less }
  "@"   { tok At }
  "%"   { tok Per }

  ( "~" | "&" | "|" | "*" | "/" | "\" | "+" | "=" | ">" | "<" | "," | "@" | "%" | "-")+ { tokT OperatorSequence decode }

  ":"   { tok Colon }

  "["   { tok NewBlock }
  "]"   { tok EndBlock }

  "#"   { tok Pound }
  "^"   { tok TExit }
  "."   { tok Period }
  ":="  { tok Assign }

  $digit+ { tokT Integer parseInt }
  $digit+ "." $digit+ { tokT Double parseDouble }

  $alpha [$alpha $digit _]* ":"    { tokT Keyword decode }
  ($alpha [$alpha $digit _]* ":")+ { tokT KeywordSequence decode }

  "'" ( "\t" | "\b" | "\n" | "\r" | "\f" | "\0" | "\'" | "\\" | ~[\' \\])* "'" { tokT STString decode }

{
data PosToken = PosToken AlexPosn Token
  deriving (Show, Eq)

tok :: Token -> AlexPosn -> ByteString ->  PosToken
tok tk pos _ = PosToken pos tk

tokT :: (a -> Token) -> (ByteString -> a) -> AlexPosn -> ByteString -> PosToken
tokT tk trans pos str = PosToken pos (tk (trans str))

}
