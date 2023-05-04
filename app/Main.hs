{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Main (main) where

import Control.Eff
import Control.Eff.Exception
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import HaSOM.AST.PrettyPrint (prettyPrintAST)
import HaSOM.Lexer.Alex (prettyPrintTokens)
import HaSOM.Run
import System.Console.CmdArgs
import System.Exit (ExitCode (ExitFailure), exitWith)

data HaSOM
  = Scan {filepath :: FilePath}
  | Parse {filepath :: FilePath}
  | Compile {classpath :: [FilePath], time :: Bool}
  | Exec
      { mainClass :: Text,
        classpath :: [FilePath],
        arguments :: [Text],
        time :: Bool,
        trace :: Bool
      }
  deriving (Show, Data, Typeable)

scan =
  record Scan {} [filepath := def += typFile += argPos 0]
    += help "Scan the file and print the tokens"

parse =
  record Parse {} [filepath := def += typFile += argPos 0]
    += help "Parse the file and print the abstract syntax tree"

compile =
  record
    Compile {}
    [ classpath := def += args += typ "FILES/DIRS",
      time := def += help "Enable time measurement"
    ]
    += help "Compile the files on given filepaths and print the disassembled initial runtime"

exec =
  record
    Exec {}
    [ classpath := def += typ "FILES/DIRS",
      mainClass := "" += argPos 0 += typ "mainClass",
      arguments := def += args += typ "ARGS",
      time := def += help "Enable time measurement",
      trace := def += help "Enable VM tracing"
    ]
    += auto
    += help "Execute the SOM program at given classpaths with given main"

mode :: Mode (CmdArgs HaSOM)
mode =
  cmdArgsMode_ $
    modes_ [scan, parse, compile, exec]
      += program "HaSOM"
      += summary "A SOM compiler and virtual machine"

wrap :: Eff [Exc Int, Lift IO] a -> IO a
wrap f = do
  runLift (runError f) >>= \case
    Left e -> exitWith (ExitFailure e)
    Right a -> pure a

main :: IO ()
main = do
  cmdArgsRun mode
    >>= \case
      Scan fp ->
        wrap (doScan fp)
          >>= TIO.putStrLn . prettyPrintTokens
      Parse fp ->
        wrap (doParse fp)
          >>= TIO.putStrLn . prettyPrintAST
      Compile {..} ->
        wrap (doCompile time classpath)
          >>= TIO.putStrLn . doDisassemble
      Exec {..} ->
        wrap (doCompile time classpath)
          >>= doExecute mainClass arguments trace time
