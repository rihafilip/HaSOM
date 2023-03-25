import qualified Combinator.Spec
import qualified HaSOM.Lexer.Alex.Golden
import qualified HaSOM.Parser.Happy.Golden
import Test.Hspec (hspec)
import WithSources

main :: IO ()
main = do
  testFiles <- loadTestFiles "source-tests"
  hspec $ do
    Combinator.Spec.spec

    -- Golden tests
    sequenceTestFilesGolden
      testFiles
      [ HaSOM.Lexer.Alex.Golden.golden,
        HaSOM.Parser.Happy.Golden.golden
      ]
