module HaSOM.Parser.Happy.Golden(golden) where
import WithSources
import HaSOM.Parser.Happy (parse)
import HaSOM.Lexer.Alex (alexScanTokens)
import HaSOM.AST.PrettyPrint (prettyPrintAST)

golden :: SourceGolden
golden = ("Happy", prettyPrintAST . parse . alexScanTokens)
