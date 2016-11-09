module Main where

import Parser
import Semant

import System.Environment
import System.Console.GetOpt

import Control.Monad


data Flag = Parse | Semant deriving Show

options :: [OptDescr Flag]
options = [
  Option "p" ["parse"] (NoArg Parse) "Parse a program and print out an AST."
  , Option "s" ["semant"] (NoArg Semant) "Parse a program and run semantic analysis. Print out an annotated AST."
  ]

getOptions :: [String] -> IO (Flag, [FilePath])
getOptions argv =
  case getOpt Permute options argv of
    ([option], inputFiles, _) -> return (option, inputFiles)
    (_:_:_, _, _) -> error "Too many options are passed"
    ([], _, []) -> error "Option that specifies mode is required"
    (_, _, errorMsg) -> error $ concat errorMsg

runParserInner file = do
  result <- parseFile file
  case result of
    (Right ast) -> return ast
    (Left err) -> error $ show err

runParser :: FilePath -> IO ()
runParser file = do
  ast <- runParserInner file
  putStrLn $ show ast

runSemant :: FilePath -> IO ()
runSemant file = do
  ast <- runParserInner file
  case semant ast of
    (Right ()) -> putStrLn $ show ast
    (Left err) -> error $ show err

main :: IO ()
main = do
  argv <- getArgs
  (option, files) <- getOptions argv
  case option of
    Parse -> forM_ files runParser
    Semant -> forM_ files runSemant
