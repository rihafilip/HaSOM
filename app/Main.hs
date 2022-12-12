module Main (main) where

import HaSOM.Parser.Happy
import HaSOM.Lexer.Alex
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM_ lexAndParse

lexAndParse file = do
  putStrLn $ "File: " ++ file

  content <- readFile file
  let tokens = alexScanTokens content
  mapM_ print tokens

  putStrLn ""

  let ast = parse tokens
  print ast

  putStrLn "--------------"
