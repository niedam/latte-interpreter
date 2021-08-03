module Main where

import System.Environment (getArgs)
import Latte.Lex   (Token)
import Latte.Par   (pProgram, myLexer)
import Latte.TypeChecking (checkTypes)
import Latte.StaticAnalysis (staticAnalysis)
import Latte.RunTime (runProgram)
import System.IO.Error
import qualified Latte.Error as Error
import qualified Latte.Abs as Abs


parse :: String -> IO Abs.Program
parse s = do
  let pTree = pProgram $ myLexer s
  case pTree of
    Left err -> Error.parseError err
    Right tree -> return tree


run :: String -> IO ()
run s = do
  tree <- parse s
  checkTypes tree
  staticAnalysis tree
  runProgram tree

usage = putStrLn "usage: Call with one argument: path to script."

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [fs] -> catchIOError (readFile fs >>= run) Error.openFileError
    _    -> Error.noScript
