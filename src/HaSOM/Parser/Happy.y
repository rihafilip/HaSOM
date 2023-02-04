{
-- | Happy generated parser
module HaSOM.Parser.Happy(parse) where

import HaSOM.Parser.ParseTree
import qualified HaSOM.AST as AST
import HaSOM.Lexer.Token

import Data.List.NonEmpty (NonEmpty(..))

}

%name parse
%tokentype { Token }

%token
  Primitive   { TPrimitive }
  Identifier  { Identifier $$ }
  Equal       { Equal }
  Separator   { Separator }
  NewTerm     { NewTerm }
  EndTerm     { EndTerm }
  Or          { Or }
  Comma       { Comma }
  Minus       { Minus }
  Not         { Not }
  And         { And }
  Star        { Star }
  Div         { Div }
  Mod         { Mod }
  Plus        { Plus }
  More        { More }
  Less        { Less }
  At          { At }
  Per         { Per }
  Colon       { Colon }
  NewBlock    { NewBlock }
  EndBlock    { EndBlock }
  Pound       { Pound }
  Exit        { TExit }
  Period      { Period }
  Assign      { Assign }
  Integer     { Integer $$ }
  Double      { Double $$ }
  Keyword     { Keyword $$ }
  STString    { STString $$ }

%%

classdef  : Identifier Equal superclass instanceFields methodStar
            Separator classFields methodStar
            EndTerm
            { AST.MkClass $1 $3 $4 $5 $7 $8 }
          | Identifier Equal superclass instanceFields methodStar
            EndTerm
            { AST.MkClass $1 $3 $4 $5 [] [] }

superclass : Identifier NewTerm { Just $1 }
           | NewTerm            { Nothing }

instanceFields : {- empty -}        { [] }
               | Or variableStar Or { $2 }

classFields : {- empty -}        { [] }
            | Or variableStar Or { $2 }

variableStar : {- empty -}           { [] }
             | variableStar variable { $2 <:> $1 }

methodStar : {- empty -}     { [] }
           | methodStar method { $2 <:> $1 }

method : pattern Equal Primitive   { AST.MkMethod $1 AST.MethodPrimitive }
       | pattern Equal methodBlock { AST.MkMethod $1 (AST.MethodBlock (transformBlock $3)) }

pattern : unaryPattern   { AST.UnaryMethod $1 }
        | keywordPattern { AST.KeywordMethod $1 }
        | binaryPattern  { uncurry AST.BinaryMethod $1 }

unaryPattern : unarySelector { $1 }

binaryPattern : binarySelector argument { ($1, $2) }

keywordPattern : keywordPatternStar keyword argument { ($2, $3) <:| $1 }
keywordPatternStar : {- empty -} { [] }
                   | keywordPatternStar keyword argument { ($2, $3) <:> $1 }

methodBlock : NewTerm blockContents EndTerm { Just $2 }
            | NewTerm EndTerm               { Nothing }

unarySelector : identifier { $1 }

binarySelector : operator { $1 }
               | operatorSequence { $1 }

operatorSequence : operator operatorSequenceStar { $1 ++ $2 }
operatorSequenceStar : {- empty -} { "" }
                     | operator operatorSequenceStar { $1 ++ $2 }

operator : Or     { "|" }
         | Comma  { "," }
         | Minus  { "-" }
         | Equal  { "=" }
         | Not    { "~" }
         | And    { "&" }
         | Star   { "*" }
         | Div    { "/" }
         | Mod    { "\\" }
         | Plus   { "+" }
         | More   { ">" }
         | Less   { "<" }
         | At     { "@" }
         | Per    { "%" }

identifier : Primitive  { "primitive" }
           | Identifier { $1 }

keyword : Keyword { $1 }

argument : variable { $1 }

blockContents : Or localDefs Or blockBody { MkBlock $2 $4 }
              | blockBody                 { MkBlock [] $1 }

localDefs : {- empty -}        { [] }
          | localDefs variable { $2 <:> $1 }

blockBody : Exit result { Exit $2 }
          | expression Period blockBody { BlockBody $1 (Just $3) }
          | expression Period { BlockBody $1 Nothing }
          | expression { BlockBody $1 Nothing }

result : expression Period { $1 }
       | expression        { $1 }

expression : assignation { uncurry Assignation $1 }
           | evaluation  { Eval $1 }

assignation : assignments evaluation { ($1, $2) }

assignments : assignmentsStar assignment { $2 <:| $1 }

assignmentsStar : {- empty -}                  { [] }
                 | assignmentsStar assignment { $2 <:> $1 }

assignment : variable Assign { $1 }

primary : variable    { PrimVar $1 }
        | nestedTerm  { PrimTerm $1 }
        | nestedBlock { PrimBlock $1 }
        | literal     { PrimLit $1 }

variable : identifier { $1 }

{- inlined messages -}
evaluation : primary unaryMessagePlus binaryMessageStar keywordMessageStar
            { MkEvaluation $1 $2 $3 $4 }
           | primary binaryMessagePlus keywordMessageStar
            { MkEvaluation $1 [] $2 $3 }
           | primary keywordMessagePlus
            { MkEvaluation $1 [] [] $2 }
           | primary
            { MkEvaluation $1 [] [] [] }


unaryMessagePlus : unaryMessageStar unaryMessage { $2 <:> $1 }
unaryMessageStar : {- empty -}                   { [] }
                 | unaryMessageStar unaryMessage { $2 <:> $1 }

binaryMessagePlus : binaryMessageStar binaryMessage { $2 <:> $1 }
binaryMessageStar : {- empty -}                     { [] }
                  | binaryMessageStar binaryMessage { $2 <:> $1 }

keywordMessagePlus : keywordMessageStar keywordMessage  { $2 <:> $1 }
keywordMessageStar : {- empty -}                        { [] }
                   | keywordMessageStar keywordMessage  { $2 <:> $1 }

unaryMessage : unarySelector { $1 }

binaryMessage : binarySelector binaryOperand { MkBinaryMessage $1 $2 }

binaryOperand : primary unaryMessageStar { MkBinaryOperand $1 $2 }

keywordMessage : keywordFormulaStar keyword formula
                 { MkKeywordMessage ( (MkKeywordFormula $2 $3 ) <:| $1 ) }

keywordFormulaStar : {- empty -}
                    { [] }
                   | keywordFormulaStar keyword formula
                    { (MkKeywordFormula $2 $3) <:> $1 }

formula : binaryOperand binaryMessageStar { MkFormula $1 $2 }

nestedTerm : NewTerm expression EndTerm { $2 }

literal : literalArray { LArray $1 }
        | literalSymbol { LSymbol $1 }
        | literalString { LString $1 }
        | literalNumber { LNumber $1 }

literalArray : Pound NewTerm literalStar EndTerm { $3 }

literalStar : {- empty -}         { [] }
            | literalStar literal { $2 <:> $1 }

literalNumber : negativeDecimal { $1 }
              | literalDecimal  { $1 }

literalDecimal : literalInteger { $1 }
               | literalDouble  { $1 }

negativeDecimal : Minus literalDecimal { NMinus $2 }

literalInteger : Integer { NInteger $1 }

literalDouble : Double { NDouble $1 }

{- inlined selector -}
literalSymbol : Pound string          { AST.Symbol $2 }
              | Pound binarySelector  { AST.SBinSelector $2 }
              | Pound keywordSelector { AST.SKWSelector $2 }
              | Pound unarySelector   { AST.SUnSelector $2 }

{- inlined KeywordSequence -}
keywordSelector : Keyword { $1 <:| [] }
                | keywordStar Keyword { $2 <:| $1 }

keywordStar : {- empty -}         { [] }
            | keywordStar Keyword { $2 <:> $1 }

literalString : string { $1 }

string : STString { $1 }

nestedBlock : NewBlock blockPattern blockContents EndBlock
              { MkNestedBlock $2 (Just $3) }
            | NewBlock blockPattern EndBlock
              { MkNestedBlock $2 Nothing }
            | NewBlock blockContents EndBlock
              { MkNestedBlock [] (Just $2) }
            | NewBlock EndBlock
              { MkNestedBlock [] Nothing }

blockPattern : blockArgumentsStar Colon argument Or { $3 <:> $1 }

blockArgumentsStar : {- empty -}                       { [] }
                   | blockArgumentsStar Colon argument { $3 <:> $1 }

{
infixr 5 <:>
(<:>) :: a -> [a] -> [a]
x <:> [] = [x]
x <:> (xs : xss) = xs : (x <:> xss)

infixr 5 <:|
(<:|) :: a -> [a] -> NonEmpty a
x <:| [] = x :| []
x <:| (xs:xss) = xs :| (x <:> xss)

-- TODO make more robust
happyError s = error $ "parse error on " ++ show s

}
