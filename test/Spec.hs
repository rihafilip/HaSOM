import qualified Combinator.Spec
import qualified Data.Stack.Spec
import qualified HaSOM.Lexer.Alex.Golden
import qualified HaSOM.Parser.Happy.Golden
import Test.Hspec (hspec)
import WithSources
import qualified HaSOM.VM.Primitive.VMArray.Spec

main :: IO ()
main = do
  testFiles <- loadTestFiles "source-tests"
  hspec $ do
    Combinator.Spec.spec
    Data.Stack.Spec.spec
    HaSOM.VM.Primitive.VMArray.Spec.spec

    -- Golden tests
    sequenceTestFilesGolden
      testFiles
      [ HaSOM.Lexer.Alex.Golden.golden,
        HaSOM.Parser.Happy.Golden.golden
      ]
