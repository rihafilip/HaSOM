import qualified Combinator.Spec
import qualified Data.Stack.Spec
import qualified HaSOM.Lexer.Alex.Golden
import qualified HaSOM.Parser.Happy.Golden
import qualified HaSOM.VM.GC.Spec
import qualified HaSOM.VM.VMArray.Spec
import Test.Hspec (hspec)
import WithSources
import qualified Data.LookupMap.Spec

main :: IO ()
main = do
  testFiles <- loadTestFiles "source-tests"
  hspec $ do
    Combinator.Spec.spec
    Data.Stack.Spec.spec
    HaSOM.VM.VMArray.Spec.spec
    HaSOM.VM.GC.Spec.spec
    Data.LookupMap.Spec.spec

    -- Golden tests
    sequenceTestFilesGolden
      testFiles
      [ HaSOM.Lexer.Alex.Golden.golden,
        HaSOM.Parser.Happy.Golden.golden
      ]
