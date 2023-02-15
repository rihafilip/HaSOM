{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module HaSOM.AST.PrettyPrint (prettyPrintAST) where

import Control.Eff
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (fromMaybe)
import HaSOM.AST
import HaSOM.AST.Algebra

type IndentState r = Member (State Int) r

type PrettyPrintEff r = [State Int, Writer String] <:: r

-----------------------------------------------

prettyPrintAST :: Class -> String
prettyPrintAST =
  unlines
    . run
    . execListWriter
    . evalState (0 :: Int)
    . fold prettyPrintAlgerba

-----------------------------------------------

indented :: IndentState r => Eff r () -> Eff r ()
indented f = do
  (currIndent :: Int) <- get
  put (currIndent + 1)
  f
  put currIndent

addLine :: PrettyPrintEff r => String -> Eff r ()
addLine str = do
  (indentSize :: Int) <- get
  let indentatiton = concat (replicate indentSize "  ")
  tell $ indentatiton ++ str

(.:) :: PrettyPrintEff r => String -> String -> Eff r ()
name .: value = addLine (name ++ ": " ++ value)

-----------------------------------------------

-- | Print bracketed single line
(<#>) :: PrettyPrintEff r => String -> String -> Eff r ()
name <#> field = addLine $ "(" ++ name ++ " " ++ field ++ ")"

-- | Print bracketed multiline
--
-- It formats the input as follows:
--
-- @
-- (name field1 field2 ...
--   subexpr
--   ...
-- )
-- @
bracketMultiline :: PrettyPrintEff r => String -> [String] -> Eff r () -> Eff r ()
bracketMultiline name fields subexpr = do
  addLine $ "(" ++ name ++ " " ++ unwords fields
  indented subexpr
  addLine ")"

-----------------------------------------------

formatList :: [String] -> String
formatList list = "[ " ++ unwords list ++ " ]"

quote :: String -> String
quote = ("\"" ++) . (++ "\"")

-----------------------------------------------

prettyPrintAlgerba :: PrettyPrintEff r => Algebra (Eff r ()) (Eff r ()) (Eff r ()) (Eff r ())
prettyPrintAlgerba = MkAlgebra {..}
  where
    labeled (label, xs) = do
      addLine label
      mapM_ indented xs

    clazz name superclass instanceFields instanceMethods classFields classMethods = do
      "name" .: quote name
      "superclass" .: maybe "none" quote superclass

      mapM_
        labeled
        [ ("instance variables:", [addLine $ formatList instanceFields]),
          ("instance methods:", instanceMethods),
          ("class variables:", [addLine $ formatList classFields]),
          ("class methods:", classMethods)
        ]

    method typ body = do
      formatType typ $
        fromMaybe
          (addLine $ quote "primitive")
          body

    formatType (UnaryMethod m) body = bracketMultiline "UnaryMethod" [quote m] body
    formatType (BinaryMethod m var) body = bracketMultiline "BinaryMethod" [quote m, var] body
    formatType (KeywordMethod kwVars) body = bracketMultiline "KeywordMethod" [] (mapKwVars >> body)
      where
        mapKwVars =
          bracketMultiline
            "Keywords"
            []
            (sequence_ [kw <#> var | (kw, var) <- kwVars])

    block vars exprs = do
      case vars of
        [] -> pure ()
        _ -> "Variables" <#> formatList vars
      sequence_ exprs

    exit expr = bracketMultiline "Return" [] expr

    assign vars expr = bracketMultiline "Assign" [formatList $ NonEmpty.toList vars] expr

    unCall primary selector = bracketMultiline "UnaryCall" [quote selector] primary
    binCall primary selector secondary =
      bracketMultiline
        "BinaryCall"
        [quote selector]
        (primary >> secondary)

    kwCall primary kwExprs = bracketMultiline "KeywordCall" [] (sequence_ (printPrimary : printKws))
      where
        printPrimary = bracketMultiline "Primary" [] primary
        printKws = map onKwExpr (NonEmpty.toList kwExprs)
        onKwExpr (kw, expr) = bracketMultiline "Keyword" [quote kw] expr

    variableExpr var = "Variable" <#> var
    nestedBlock params bl = bracketMultiline "Block" [formatList params] bl

    literal (LArray lits) = bracketMultiline "Array" [] (mapM_ literal lits)
    literal (LSymbol symbol) = "Symbol" <#> symbol
    literal (LString str) = "String" <#> str
    literal (LInteger int) = "Integer" <#> show int
    literal (LDouble double) = "Integer" <#> show double
