{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HaSOM.Compiler.Spec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as B
import Data.Either (isLeft, isRight, rights)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utility ((<+))
import qualified HaSOM.AST as AST
import HaSOM.Compiler
import HaSOM.Lexer.Alex (alexScanTokens)
import HaSOM.Parser.Happy
import HaSOM.VM.Object
import HaSOM.VM.Universe (VMGlobalsNat)
import System.FilePath (takeBaseName)
import Test.Hspec
import Control.Applicative ((<|>))

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

rightError :: Show a => Either a b -> b
rightError = either (error . show) id

spec :: [(String, B.ByteString)] -> Spec
spec stdLib =
  describe "HaSOM compiler tests" $ do
    let (names, asts, res_) = compileFiles stdLib
    it "Standard library gets compiled" $ do
      case res_ of
        Left _ -> res_ `shouldSatisfy` isRight
        Right MkCompilationResult {} -> pure ()

    it "Core library are interned correctly" $ do
      let MkCompilationResult {..} = rightError res_
      let validate name =
            let (_, globalIx) = internGlobal name globals
                globalName = getGlobalName globalIx globals
             in globalName `shouldBe` Just name

      forM_ names $ validate . T.pack
      forM_ names $ validate . T.pack . (++ " class")

    it "Core library classes are classes" $ do
      let MkCompilationResult {..} = rightError res_
      let validate name =
            (globalObj, name) `shouldSatisfy` \case
              (Just (ClassGlobal _), _) -> True
              _ -> False
            where
              (_, globalIx) = internGlobal name globals
              globalObj = getGlobal globalIx globals

      forM_ names $ validate . T.pack
      forM_ names $ validate . T.pack . (++ " class")

    it "Core classes are present and valid" $ do
      let MkCompilationResult {..} = rightError res_
      forM_ coreClassesWithSelector $ \(name, selector) -> do
        let (_, globalIx) = internGlobal name globals
        globalIx `shouldBe` selector coreClasses

    it "Core classes have correct class structure" $ do
      let MkCompilationResult {..} = rightError res_
      forM_ (rights asts) $ validateAst globals

    it "Creates class as object correctly" $ do
      let MkCompilationResult {..} = rightError res_
      let idxs = map fst heap
      forM_ names $ \name -> do
        let (_, globalIx) = internGlobal (T.pack name) globals
        let ClassGlobal MkVMClass{asObject} = fromJust $ getGlobal globalIx globals
        idxs `shouldContain` [asObject]

    it "Detects cyclical dependencies" $ do
      let (_, _, res) = compileFiles $ stdLib ++ [("", "Foo = Bar ()"), ("", "Bar = Foo ()")]
      res `shouldSatisfy` isLeft

    it "Detects missing superclass" $ do
      let (_, _, res) = compileFiles $ stdLib ++ [("", "Foo = Bar ()")]
      res `shouldSatisfy` isLeft

validateAst :: VMGlobalsNat -> AST.Class -> Expectation
validateAst globals ast = do
  let expectedSuperclassName =
        case AST.superclass ast of
          Nothing -> Just "Object"
          Just "nil" -> Nothing
          s -> s

  let (_, globalIx) = internGlobal (AST.name ast) globals
  let ClassGlobal vmclass = fromJust $ getGlobal globalIx globals

  (superclass vmclass >>= (`getGlobalName` globals))
    `shouldBe` expectedSuperclassName

  let (_, metaglobalIx) = internGlobal (AST.name ast <+ " class") globals
  let ClassGlobal vmmetaclass = fromJust $ getGlobal metaglobalIx globals

  (superclass vmmetaclass >>= (`getGlobalName` globals))
    `shouldBe` ((<+ " class") <$> expectedSuperclassName) <|> Just "Class"
