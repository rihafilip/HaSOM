{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaSOM.Run (doScan, doParse, doCompile, doDisassemble, doExecute) where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Reader.Strict
import Control.Eff.State.Strict
import qualified Data.ByteString.Lazy as B
import Data.Functor ((<&>))
import Data.Stack (emptyStack)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Utility
import qualified HaSOM.AST as AST
import HaSOM.Compiler
import HaSOM.Interpreter
import HaSOM.Lexer.Alex
import HaSOM.Parser.Happy
import HaSOM.VM.Disassembler
import qualified HaSOM.VM.GC as GC
import HaSOM.VM.Primitive (defaultPrimitives)
import HaSOM.VM.Universe
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Directory.Recursive (getFilesRecursive)
import System.Exit (ExitCode (ExitFailure), exitFailure, exitSuccess, exitWith)
import System.FilePath (takeExtension)
import System.IO (stderr)

type ExecEff r = (Lifted IO r, Member (Exc Int) r)

tryEff :: ExecEff r => Text -> Int -> Either Text a -> Eff r a
tryEff errM errC =
  either
    exitW
    pure
  where
    exitW err = lift (TIO.hPutStrLn stderr $ errM <+ err) >> throwError errC

doScan :: ExecEff r => FilePath -> Eff r [PosToken]
doScan fp = do
  cont <- lift $ B.readFile fp
  lift (scan cont)
    >>= tryEff
      ("Lexer error in file " <+ T.pack fp <+ ": ")
      101

doParse :: ExecEff r => FilePath -> Eff r AST.Class
doParse fp = do
  tokens <- doScan fp
  tryEff
    ("Parser error in file " <+ T.pack fp <+ " : ")
    102
    (parse tokens)

doCompile :: ExecEff r => [FilePath] -> Eff r CompilationResult
doCompile files = do
  classpaths <-
    lift $
      filter ((==) ".som" . takeExtension)
        . concat
        <$> mapM collect files
  asts <- mapM doParse classpaths
  tryEff
    "Compilation error: "
    103
    (compile asts defaultPrimitives)
  where
    collect fp =
      doesDirectoryExist fp >>= \case
        True -> getFilesRecursive fp
        False ->
          doesFileExist fp <&> \case
            True -> [fp]
            False -> []

doDisassemble :: CompilationResult -> Text
doDisassemble MkCompilationResult {globals, literals} =
  run $ evalState literals $ evalState globals $ do
    gls <- disassembleGlobals globals
    lits <- disassembleLiterals literals
    pure $ lits <+ "\n\n" <+ gls

doExecute :: Text -> [Text] -> Bool -> CompilationResult -> IO ()
doExecute clazz args trace MkCompilationResult {..} =
  do
    let cs = (emptyStack :: CallStackNat)
    let os = (emptyStack :: ObjStack)
    let gc = (GC.fromList nilObj heap :: GCNat)

    (((res, fGC), fCs), fOs) <-
      runLift $
        runState os $
          evalState globals $
            runReader coreClasses $
              runReader (traceFromBool trace) $
                runState cs $
                  evalState literals $
                    runState gc $
                      runError @Text
                        (bootstrap clazz args >> interpret)

    case res of
      Left txt -> do
        TIO.hPutStrLn stderr ("Runtime error: " <+ txt)
        if trace
          then do
            putStrLn ""
            putStrLn "Stack trace:"
            runLift (disassembleStack fGC fOs) >>= TIO.putStrLn
            putStrLn "Call stack trace:"
            disassembleCallStack fGC fCs >>= TIO.putStrLn
          else do
            putStrLn ""
            putStrLn "Stack trace:"
            runLift (stackTrace fCs) >>= TIO.putStrLn
        exitFailure
      Right 0 -> exitSuccess
      Right n -> exitWith (ExitFailure n)
