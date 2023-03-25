{
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Alex generated lexer
module HaSOM.Lexer.Alex(alexScanTokens) where

import HaSOM.Lexer.Token
import HaSOM.Lexer.Alex.Parsing

}

%wrapper "basic-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$escaped = [\\t \\b \\n \\r \\f \\0 \\\' \\\\]

tokens :-
  \" (\n | ~\")* \" ;
  $white+ ;

  "primitive" { \_ -> TPrimitive }
  $alpha [$alpha $digit _]* { Identifier . decode }

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
  "\"   { \_ -> Mod }
  "+"   { \_ -> Plus }
  ">"   { \_ -> More }
  "<"   { \_ -> Less }
  "@"   { \_ -> At }
  "%"   { \_ -> Per }

  ( "~" | "&" | "|" | "*" | "/" | "\" | "+" | "=" | ">" | "<" | "," | "@" | "%" | "-")+ { OperatorSequence . decode }

  ":"   { \_ -> Colon }

  "["   { \_ -> NewBlock }
  "]"   { \_ -> EndBlock }

  "#"   { \_ -> Pound }
  "^"   { \_ -> TExit }
  "."   { \_ -> Period }
  ":="  { \_ -> Assign }

  $digit+ { Integer . parseInt }
  $digit+ "." $digit+ { Double . parseDouble }

  $alpha [$alpha $digit _]* ":"    { Keyword . decode }
  ($alpha [$alpha $digit _]* ":")+ { KeywordSequence . decode }

  "'" ($escaped | ~[\' \\])* "'" { STString . decode }

{

}
