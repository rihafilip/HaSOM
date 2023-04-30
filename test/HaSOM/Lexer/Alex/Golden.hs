module HaSOM.Lexer.Alex.Golden(golden) where

import HaSOM.Lexer.Alex
import WithSources (SourceGolden)
import qualified Data.Text as T
import HaSOM.Lexer.Token (tokenToText)

golden :: SourceGolden
golden = ("Alex", T.unlines . map (\(PosToken _ tk) -> tokenToText tk) . alexScanTokens)
