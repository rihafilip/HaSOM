module HaSOM.Parser.Happy.Golden (golden) where

import HaSOM.AST.PrettyPrint (prettyPrintAST)
import HaSOM.Lexer.Alex (alexScanTokens)
import HaSOM.Parser.Happy (parse)
import WithSources

golden :: SourceGolden
golden = ("Happy", either id prettyPrintAST . parse . alexScanTokens)
