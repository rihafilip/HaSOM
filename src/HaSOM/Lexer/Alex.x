{
-- | Alex generated lexer
module HaSOM.Lexer.Alex(alexScanTokens) where

import HaSOM.Lexer.Token
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$escaped = [\\t \\b \\n \\r \\f \\0 \\\' \\\\]

tokens :-
  \" (\n | ~\")* \" ;
  $white+ ;

  "primitive" { \_ -> TPrimitive }
  $alpha [$alpha $digit _]* { Identifier }

  "=" { \_ -> Equal }

  "----" "-"* { \_ -> Separator }

  "(" { \_ -> NewTerm }
  ")" { \_ -> EndTerm }
  "|" { \_ -> Or }

  ","   { \_ -> Comma }
  "-"   { \_ -> Minus }
  "~"   { \_ -> Not }
  "&"   { \_ -> And }
  "*"   { \_ -> Star }
  "/"   { \_ -> Div }
  "\\"  { \_ -> Mod }
  "+"   { \_ -> Plus }
  ">"   { \_ -> More }
  "<"   { \_ -> Less }
  "@"   { \_ -> At }
  "%"   { \_ -> Per }

  ( "~" | "&" | "|" | "*" | "/" | "\\" | "+" | "=" | ">" | "<" | "," | "@" | "%" | "-")+ { OperatorSequence }

  ":"   { \_ -> Colon }

  "["   { \_ -> NewBlock }
  "]"   { \_ -> EndBlock }

  "#"   { \_ -> Pound }
  "^"   { \_ -> TExit }
  "."   { \_ -> Period }
  ":="  { \_ -> Assign }

  $digit+ { Integer . read }
  $digit+ "." $digit+ { Double . read }

  $alpha [$alpha $digit _]* ":"    { Keyword }
  ($alpha [$alpha $digit _]* ":")+ { KeywordSequence }

  "'" ($escaped | ~[\' \\])* "'" { STString }

{

}
