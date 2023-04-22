import qualified Combinator.Spec
import qualified Data.Stack.Spec
import qualified HaSOM.Lexer.Alex.Golden
import qualified HaSOM.Parser.Happy.Golden
import qualified HaSOM.VM.GC.Spec
import qualified HaSOM.VM.VMArray.Spec
import Test.Hspec (hspec)
import WithSources

main :: IO ()
main = do
  testFiles <- loadTestFiles "source-tests"
  hspec $ do
    Combinator.Spec.spec
    Data.Stack.Spec.spec
    HaSOM.VM.VMArray.Spec.spec
    HaSOM.VM.GC.Spec.spec

    -- Golden tests
    sequenceTestFilesGolden
      testFiles
      [ HaSOM.Lexer.Alex.Golden.golden,
        HaSOM.Parser.Happy.Golden.golden
      ]
