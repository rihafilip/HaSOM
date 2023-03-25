{
-- | Happy generated parser
module HaSOM.Parser.Happy(parse) where

import HaSOM.Parser.ParseTree
import qualified HaSOM.AST as AST
import HaSOM.Lexer.Token

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

}

%name parse
%tokentype { Token }

%token
  Primitive         { TPrimitive }
  Identifier        { Identifier $$ }
  Equal             { Equal }
  Separator         { Separator }
  NewTerm           { NewTerm }
  EndTerm           { EndTerm }
  Or                { Or }
  Comma             { Comma }
  Minus             { Minus }
  Not               { Not }
  And               { And }
  Star              { Star }
  Div               { Div }
  Mod               { Mod }
  Plus              { Plus }
  More              { More }
  Less              { Less }
  At                { At }
  Per               { Per }
  OperatorSequence  { OperatorSequence $$ }
  Colon             { Colon }
  NewBlock          { NewBlock }
  EndBlock          { EndBlock }
  Pound             { Pound }
  Exit              { TExit }
  Period            { Period }
  Assign            { Assign }
  Integer           { Integer $$ }
  Double            { Double $$ }
  Keyword           { Keyword $$ }
  KeywordSequence   { KeywordSequence $$ }
  STString          { STString $$ }

%%

classdef :: { AST.Class }
classdef  : Identifier Equal superclass instanceFields methodStar
            Separator classFields methodStar
            EndTerm
            { AST.MkClass $1 $3 $4 $5 $7 $8 }
          | Identifier Equal superclass instanceFields methodStar
            EndTerm
            { AST.MkClass $1 $3 $4 $5 [] [] }

superclass :: { Maybe AST.Identifier }
superclass : Identifier NewTerm { Just $1 }
           | NewTerm            { Nothing }

instanceFields :: { [AST.Variable] }
instanceFields : {- empty -}        { [] }
               | Or variableStar Or { $2 }

classFields :: { [AST.Variable] }
classFields : {- empty -}        { [] }
            | Or variableStar Or { $2 }

variableStar :: { [AST.Variable] }
variableStar : {- empty -}           { [] }
             | variableStar variable { $2 <:> $1 }

methodStar :: { [AST.Method] }
methodStar : {- empty -}     { [] }
           | methodStar method { $2 <:> $1 }

method :: { AST.Method }
method : pattern Equal Primitive   { AST.MkMethod $1 AST.MethodPrimitive }
       | pattern Equal methodBlock { AST.MkMethod $1 (AST.MethodBlock (transformBlock $3)) }

pattern :: { AST.MethodType }
pattern : unaryPattern   { AST.UnaryMethod $1 }
        | keywordPattern { AST.KeywordMethod $1 }
        | binaryPattern  { uncurry AST.BinaryMethod $1 }

unaryPattern :: { Text }
unaryPattern : unarySelector { $1 }

binaryPattern :: { (AST.BinarySelector, AST.Variable) }
binaryPattern : binarySelector argument { ($1, $2) }

keywordPattern :: { NonEmpty (AST.Keyword, AST.Variable) }
keywordPattern : keywordPatternStar keyword argument { ($2, $3) <:| $1 }

keywordPatternStar :: { [(AST.Keyword, AST.Variable)] }
keywordPatternStar : {- empty -} { [] }
                   | keywordPatternStar keyword argument { ($2, $3) <:> $1 }

methodBlock :: { Maybe Block }
methodBlock : NewTerm blockContents EndTerm { Just $2 }
            | NewTerm EndTerm               { Nothing }

unarySelector :: { AST.UnarySelector }
unarySelector : identifier { $1 }

binarySelector :: { AST.BinarySelector }
binarySelector : operator { $1 }
               | OperatorSequence { $1 }

operator :: { Text }
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

identifier :: { AST.Identifier }
identifier : Primitive  { "primitive" }
           | Identifier { $1 }

keyword :: { AST.Keyword }
keyword : Keyword { $1 }

argument :: { AST.Variable }
argument : variable { $1 }

blockContents :: { Block }
blockContents : Or localDefs Or blockBody { MkBlock $2 $4 }
              | blockBody                 { MkBlock [] $1 }
              | Or localDefs Or           { MkBlock $2 EmptyBlockBody }

localDefs :: { [AST.Variable] }
localDefs : {- empty -}        { [] }
          | localDefs variable { $2 <:> $1 }

blockBody :: { BlockBody }
blockBody : Exit result { Exit $2 }
          | expression Period blockBody { BlockBody $1 (Just $3) }
          | expression Period { BlockBody $1 Nothing }
          | expression { BlockBody $1 Nothing }

result :: { Expression }
result : expression Period { $1 }
       | expression        { $1 }

expression :: { Expression }
expression : assignation { uncurry Assignation $1 }
           | evaluation  { Eval $1 }

assignation :: { (NonEmpty AST.Variable, Evaluation) }
assignation : assignments evaluation { ($1, $2) }

assignments :: { NonEmpty AST.Variable }
assignments : assignmentsStar assignment { $2 <:| $1 }

assignmentsStar :: { [AST.Variable] }
assignmentsStar : {- empty -}                  { [] }
                 | assignmentsStar assignment { $2 <:> $1 }

assignment :: { AST.Variable }
assignment : variable Assign { $1 }

primary :: { Primary }
primary : variable    { PrimVar $1 }
        | nestedTerm  { PrimTerm $1 }
        | nestedBlock { PrimBlock $1 }
        | literal     { PrimLit $1 }

variable :: { AST.Variable }
variable : identifier { $1 }

{- inlined messages -}
evaluation :: { Evaluation }
evaluation : primary unaryMessagePlus binaryMessageStar keywordMessageStar
            { MkEvaluation $1 $2 $3 $4 }
           | primary binaryMessagePlus keywordMessageStar
            { MkEvaluation $1 [] $2 $3 }
           | primary keywordMessagePlus
            { MkEvaluation $1 [] [] $2 }
           | primary
            { MkEvaluation $1 [] [] [] }


unaryMessagePlus :: { [AST.UnarySelector] }
unaryMessagePlus : unaryMessageStar unaryMessage { $2 <:> $1 }

unaryMessageStar :: { [AST.UnarySelector] }
unaryMessageStar : {- empty -}                   { [] }
                 | unaryMessageStar unaryMessage { $2 <:> $1 }

binaryMessagePlus :: { [BinaryMessage] }
binaryMessagePlus : binaryMessageStar binaryMessage { $2 <:> $1 }

binaryMessageStar :: { [BinaryMessage] }
binaryMessageStar : {- empty -}                     { [] }
                  | binaryMessageStar binaryMessage { $2 <:> $1 }

keywordMessagePlus :: { [KeywordMessage] }
keywordMessagePlus : keywordMessageStar keywordMessage  { $2 <:> $1 }

keywordMessageStar :: { [KeywordMessage] }
keywordMessageStar : {- empty -}                        { [] }
                   | keywordMessageStar keywordMessage  { $2 <:> $1 }

unaryMessage :: { AST.UnarySelector }
unaryMessage : unarySelector { $1 }

binaryMessage :: { BinaryMessage }
binaryMessage : binarySelector binaryOperand { MkBinaryMessage $1 $2 }

binaryOperand :: { BinaryOperand }
binaryOperand : primary unaryMessageStar { MkBinaryOperand $1 $2 }

keywordMessage :: { KeywordMessage }
keywordMessage : keywordFormulaStar keyword formula
                 { MkKeywordMessage ( (MkKeywordFormula $2 $3 ) <:| $1 ) }

keywordFormulaStar :: { [KeywordFormula] }
keywordFormulaStar : {- empty -}
                    { [] }
                   | keywordFormulaStar keyword formula
                    { (MkKeywordFormula $2 $3) <:> $1 }

formula :: { Formula }
formula : binaryOperand binaryMessageStar { MkFormula $1 $2 }

nestedTerm :: { Expression }
nestedTerm : NewTerm expression EndTerm { $2 }

literal :: { Literal }
literal : literalArray { LArray $1 }
        | literalSymbol { LSymbol $1 }
        | literalString { LString $1 }
        | literalNumber { LNumber $1 }

literalArray :: { [Literal] }
literalArray : Pound NewTerm literalStar EndTerm { $3 }

literalStar :: { [Literal] }
literalStar : {- empty -}         { [] }
            | literalStar literal { $2 <:> $1 }

literalNumber :: { Number }
literalNumber : negativeDecimal { $1 }
              | literalDecimal  { $1 }

literalDecimal :: { Number }
literalDecimal : literalInteger { $1 }
               | literalDouble  { $1 }

negativeDecimal :: { Number }
negativeDecimal : Minus literalDecimal { NMinus $2 }

literalInteger :: { Number }
literalInteger : Integer { NInteger $1 }

literalDouble :: { Number }
literalDouble : Double { NDouble $1 }

{- inlined selector -}
literalSymbol :: { Text }
literalSymbol : Pound string          { $2 }
              | Pound binarySelector  { $2 }
              | Pound keywordSelector { $2 }
              | Pound unarySelector   { $2 }

keywordSelector :: { Text }
keywordSelector : Keyword { $1 }
                | KeywordSequence { $1 }

keywordStar :: { [AST.Keyword] }
keywordStar : {- empty -}         { [] }
            | keywordStar Keyword { $2 <:> $1 }

literalString :: { Text }
literalString : string { $1 }

string :: { Text }
string : STString { $1 }

nestedBlock :: { NestedBlock }
nestedBlock : NewBlock blockPattern blockContents EndBlock
              { MkNestedBlock $2 (Just $3) }
            | NewBlock blockPattern EndBlock
              { MkNestedBlock $2 Nothing }
            | NewBlock blockContents EndBlock
              { MkNestedBlock [] (Just $2) }
            | NewBlock EndBlock
              { MkNestedBlock [] Nothing }

blockPattern :: { [AST.Variable] }
blockPattern : blockArgumentsStar Colon argument Or { $3 <:> $1 }

blockArgumentsStar :: { [AST.Variable] }
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
