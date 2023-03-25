{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module WithSources(SourceGolden, sequenceTestFilesGolden, runTestFilesGolden, loadTestFiles) where

import qualified Data.ByteString.Lazy as B
import Data.Foldable (forM_)
import System.Directory.Recursive ( getFilesRecursive )
import Test.Hspec
import Test.Hspec.Golden ( Golden (..) )
import Control.Monad ((>=>))
import Data.Text ( Text )
import qualified Data.Text.IO as T

type SourceGolden = (String, B.ByteString -> Text)

sequenceTestFilesGolden :: [(String, B.ByteString)] -> [SourceGolden] -> Spec
sequenceTestFilesGolden testFiles =
  mapM_ (uncurry (runTestFilesGolden testFiles))

runTestFilesGolden :: [(String, B.ByteString)] -> String -> (B.ByteString -> Text) -> Spec
runTestFilesGolden testFiles name f =
  describe (name ++ " - Golden test") $
    forM_ testFiles $ \(filename, content) ->
      it ("File '" ++ filename ++ "'") $
        goldenText (name ++ "/" ++ filename) (f content)

loadTestFiles :: FilePath -> IO [(String, B.ByteString)]
loadTestFiles =
  getFilesRecursive
    >=> mapM (\fp' -> (fp',) <$> B.readFile fp')

goldenText :: String -> Text -> Golden Text
goldenText name actualOutput =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = T.writeFile,
    readFromFile = T.readFile,
    goldenFile = "./.golden" </> name ++ ".golden",
    actualFile = Just ("./.golden_actual" </> name ++ ".golden"),
    failFirstTime = False
  }

-- |
-- >>> "folder" </> "file"
-- "folder/file"
(</>) :: String -> String -> String
(</>) = (++) . (++ "/")
