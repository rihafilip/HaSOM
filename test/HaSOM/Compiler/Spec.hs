{-# OPTIONS_GHC -Wno-orphans #-}

module HaSOM.Compiler.Spec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight, isRight, isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Utility ((<+))
import qualified HaSOM.AST as AST
import HaSOM.Compiler
import HaSOM.Lexer.Alex (PosToken, alexScanTokens)
import HaSOM.Parser.Happy
import HaSOM.VM.Object (CoreClasses (..), GlobalIx, internGlobal)
import HaSOM.VM.Object.VMClass (VMClass (..))
import HaSOM.VM.Object.VMGlobal
import HaSOM.VM.Universe (NativeFun, VMGlobalNat, VMGlobalsNat)
import System.FilePath (takeBaseName)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import WithSources

instance Show CompilationResult where
  show = const "<CompilationResult>"

instance Show (VMGlobal f) where
  show (ClassGlobal MkVMClass {}) = "<ClassGlobal>"
  show (ObjectGlobal idx) = "ObjectGlobal{" ++ show idx ++ "}"

coreClassesWithSelector :: [(Text, CoreClasses -> GlobalIx)]
coreClassesWithSelector =
  [ ("Class", classClass),
    ("Metaclass", metaclassClass),
    ("Object", objectClass),
    ("System", systemClass),
    ("Method", methodClass),
    ("Primitive", primitiveClass),
    ("Boolean", booleanClass),
    ("Integer", integerClass),
    ("Double", doubleClass),
    ("String", stringClass),
    ("Symbol", symbolClass),
    ("Array", arrayClass),
    ("Nil", nilClass),
    ("True", trueClass),
    ("False", falseClass),
    ("Block", blockClass),
    ("Block1", block1Class),
    ("Block2", block2Class),
    ("Block3", block3Class)
  ]

compileFiles :: [(String, B.ByteString)] -> ([String], [Either Text AST.Class], Either Text CompilationResult)
compileFiles stdLib =
  let (names, contents) = unzip stdLib
      baseNames = map takeBaseName names
      asts = map (parse . alexScanTokens) contents
      compiled = sequence asts >>= (`compile` [])
   in (baseNames, asts, compiled)

extractCompiled :: Either Text CompilationResult -> CompilationResult
extractCompiled = fromRight (error "Compilation error")

spec :: [(String, B.ByteString)] -> Spec
spec stdLib =
  describe "HaSOM compiler tests" $ do
    let (names, asts, res_) = compileFiles stdLib
    it "Standard library gets compiled" $ do
      case res_ of
        Left _ -> res_ `shouldSatisfy` isRight
        Right MkCompilationResult {} -> pure ()

    it "Core library are interned correctly" $ do
      let MkCompilationResult {..} = extractCompiled res_
      let validate name =
            let (_, globalIx) = internGlobal name globals
                globalName = getGlobalName globalIx globals
             in globalName `shouldBe` Just name

      forM_ names $ validate . T.pack
      forM_ names $ validate . T.pack . (++ " class")

    it "Core library classes are classes" $ do
      let MkCompilationResult {..} = extractCompiled res_
      let validate name =
            let (_, globalIx) = internGlobal name globals
                globalObj = getGlobal globalIx globals
             in (globalObj, name) `shouldSatisfy` \case
                  (Just (ClassGlobal _), _) -> True
                  _ -> False

      forM_ names $ validate . T.pack
      forM_ names $ validate . T.pack . (++ " class")

    it "Core classes are present and valid" $ do
      let MkCompilationResult {..} = extractCompiled res_
      forM_ coreClassesWithSelector $ \(name, selector) -> do
        let (_, globalIx) = internGlobal name globals
        globalIx `shouldBe` selector coreClasses

    it "Detects cyclical dependencies" $ do
      let (_, _, res) = compileFiles [ ("", "Foo = Bar ()"), ("", "Bar = Foo ()") ]
      res `shouldSatisfy` isLeft

    it "Detects missing superclass" $ do
      let (_, _, res) = compileFiles [ ("", "Foo = Bar ()") ]
      res `shouldSatisfy` isLeft

