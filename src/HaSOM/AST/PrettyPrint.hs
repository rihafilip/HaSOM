{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- | Definition of pretty printing on AST
module HaSOM.AST.PrettyPrint (prettyPrintAST) where

import Control.Eff
import Control.Eff.State.Lazy
import Control.Eff.Writer.Lazy
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Maybe (fromMaybe)
import HaSOM.AST
import HaSOM.AST.Algebra
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Utility ((<+))

type PrettyPrintEff r = [State Int, Writer Text] <:: r

-----------------------------------------------

-- | Pretty print Class
prettyPrintAST :: Class -> Text
prettyPrintAST =
  T.unlines
    . run
    . (execListWriter :: Eff (Writer Text : r) a -> Eff r [Text] )
    . evalState (0 :: Int)
    . fold prettyPrintAlgerba

-----------------------------------------------

indented :: PrettyPrintEff r => Eff r () -> Eff r ()
indented f = do
  (currIndent :: Int) <- get
  put (currIndent + 1)
  f
  put currIndent

addLine :: PrettyPrintEff r => Text -> Eff r ()
addLine str = do
  (indentSize :: Int) <- get
  let indentatiton = T.replicate indentSize "  "
  tell $ indentatiton <+ str

(.:) :: PrettyPrintEff r => Text -> Text -> Eff r ()
name .: value = addLine (name <+ ": " <+ value)

-----------------------------------------------

-- | Print bracketed single line
(<#>) :: PrettyPrintEff r => Text -> Text -> Eff r ()
name <#> field = addLine $ "(" <+ name <+ " " <+ field <+ ")"

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
bracketMultiline :: PrettyPrintEff r => Text -> [Text] -> Eff r () -> Eff r ()
bracketMultiline name fields subexpr = do
  addLine $ "(" <+ name <+ " " <+ T.unwords fields
  indented subexpr
  addLine ")"

-----------------------------------------------

formatList :: [Text] -> Text
formatList list = "[ " <+ T.unwords list <+ " ]"

quote :: Text -> Text
quote = ("\"" <+) . (<+ "\"")

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
    literal (LInteger int) = "Integer" <#> T.pack (show int)
    literal (LDouble double) = "Integer" <#> T.pack (show double)
