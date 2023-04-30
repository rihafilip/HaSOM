{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.Run where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import Control.Exception (ErrorCall, evaluate, try)
import qualified Data.ByteString.Lazy as B
import Data.Functor ((<&>))
import Data.Stack (emptyStack)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Text.Utility
import qualified HaSOM.AST as AST
import HaSOM.Compiler
import HaSOM.Lexer.Alex
import HaSOM.Lexer.Token (Token)
import HaSOM.Parser.Happy
import HaSOM.VM.Disassembler
import qualified HaSOM.VM.GC as GC
import HaSOM.VM.Interpreter
import HaSOM.VM.Primitive (defaultPrimitives)
import HaSOM.VM.Universe
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Directory.Recursive (getFilesRecursive)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitWith)
import System.IO (hPutStrLn, stderr)

type ExecEff r = (Lifted IO r, Member (Exc Int) r)

tryError :: ExecEff r => String -> Int -> a -> Eff r a
tryError errM errC f = do
  lift (try @ErrorCall $ evaluate f) >>= \case
    Left err -> errorOut errM errC err
    Right x -> pure x

errorOut :: (ExecEff r, Show s) => String -> Int -> s -> Eff r a
errorOut errM errC err = lift (hPutStrLn stderr $ errM ++ show err) >> throwError errC

doScan :: ExecEff r => FilePath -> Eff r [PosToken]
doScan fp = do
  cont <- lift $ B.readFile fp
  tryError "Lexer error: " 101 (alexScanTokens cont)

doParse :: ExecEff r => FilePath -> Eff r AST.Class
doParse fp = do
  tokens <- doScan fp
  either
    (errorOut "Parser error: " 102)
    pure
    (parse tokens)

doCompile :: ExecEff r => [FilePath] -> Eff r CompilationResult
doCompile files = do
  classpaths <- lift $ concat <$> mapM collect files
  asts <- mapM doParse classpaths
  either
    (errorOut "Compilation error: " 103)
    pure
    (compile asts (GC.empty undefined) defaultPrimitives) -- TODO gc
  where
    collect fp =
      doesDirectoryExist fp >>= \case
        True -> getFilesRecursive fp
        False ->
          doesFileExist fp <&> \case
            True -> [fp]
            False -> []

doDissasemble :: CompilationResult -> Text
doDissasemble MkCompilationResult {globals, literals} =
  let gl = run $ evalState literals $ disassembleGlobals globals
   in disassembleLiterals literals
        <+ "\n\n"
        <+ gl

doExecute :: Text -> [Text] -> CompilationResult -> IO ()
doExecute clazz args MkCompilationResult {globals, coreClasses, literals, gCollector} =
  do
    let cs = (emptyStack :: CallStackNat)
    let os = (emptyStack :: ObjStack)

    (((((res, fGC), fLits), fCallSt), fGlobs), fStack) <-
      runLift $
        runState os $
          runState globals $
            runReader coreClasses $
              runState cs $
                runState literals $
                  runState gCollector $
                    runError @Text
                      (bootstrap clazz args >> interpret)

    case res of
      Left txt -> TIO.hPutStrLn stderr ("Runtime error: " <+ txt) >> exitFailure
      Right n -> exitWith (ExitFailure n)
