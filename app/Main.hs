module Main where

import Parser
import Semant

import System.Console.Haskeline
import Control.Monad.Trans
import Data.List

loadFileCommand = ":load"

processLine :: String -> IO ()
processLine line = do
  if loadFileCommand `isPrefixOf` line
    then loadFile $ drop (length loadFileCommand + 1) line
    else eval line

loadFile :: String -> IO ()
loadFile filePath = do
  parseResult <- parseFile filePath
  case parseResult of
    Left error -> putStrLn $ show error
    Right ast -> do
      case semant ast of
        Right () -> putStrLn $ show ast
        Left err -> putStrLn $ show err

eval :: String -> IO ()
eval line = do
  case parseTopLevel line of
    Left error -> putStrLn $ show error
    Right expr -> putStrLn $ show expr

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          line <- getInputLine ">"
          case line of
            Nothing -> outputStrLn "Bye!"
            Just input -> liftIO (processLine input) >> loop
