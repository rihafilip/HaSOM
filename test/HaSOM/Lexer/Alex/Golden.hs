module HaSOM.Lexer.Alex.Golden(golden) where

import HaSOM.Lexer.Alex
import WithSources (SourceGolden)
import Data.Text.Utility (showT)
import qualified Data.Text as T

golden :: SourceGolden
golden = ("Alex", T.unlines . map (\(PosToken _ tk) -> showT tk) . alexScanTokens)
