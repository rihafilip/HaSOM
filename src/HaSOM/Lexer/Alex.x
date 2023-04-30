{
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TypeApplications #-}

-- | Alex generated lexer
module HaSOM.Lexer.Alex(alexScanTokens, scan, PosToken(..), AlexPosn(..), prettyPrintTokens) where

import HaSOM.Lexer.Token
import HaSOM.Lexer.Alex.Parsing
import Control.Exception (ErrorCall, evaluate, try)
import qualified Data.Bifunctor as Bf
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utility

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

prettyPrintTokens :: [PosToken] -> Text
prettyPrintTokens = T.unlines . map mTk . snd . foldl fTk (0, [])
  where
    fTk (prev, acc) (PosToken (AlexPn _ line _) tk) =
      (line, acc ++ [ (indicator, tk) ])
      where
        indicator | line == prev = Nothing
                  | otherwise = Just line

    mTk (line, tk) =
      T.justifyRight 3 ' ' (maybe "" showT line)
      <+ "  "
      <+ tokenToText tk

scan :: ByteString -> IO (Either Text [PosToken])
scan = fmap (Bf.first showT) . try @ErrorCall . evaluate . alexScanTokens

}
