module Main where

import Parser
import System.Console.Haskeline
import Control.Monad.Trans

processLine :: String -> IO ()
processLine line = do
  case parseExpr line of
    Left error -> putStrLn $ show error
    Right expr -> putStrLn $ show expr

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          line <- getInputLine ">"
          case line of
            Nothing -> outputStrLn "Bye!"
            Just input -> liftIO (processLine input) >> loop
