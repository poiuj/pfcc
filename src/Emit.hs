module Emit where

import LLVM.Module
import LLVM.AST as AST
import LLVM.Context

import Control.Monad.Except

myModule :: AST.Module
myModule = defaultModule { moduleName = "test empty module" }

printModule :: LLVM.Module.Module -> IO String
printModule = moduleLLVMAssembly

runCodeGen :: ExceptT String IO String -> IO String
runCodeGen e = do
  result <- runExceptT e
  case result of
    Right code -> return code
    Left error -> putStrLn error >> return ""

genWithContext :: Context -> IO String
genWithContext ctx =
  runCodeGen $ withModuleFromAST ctx myModule printModule

codeGenMock :: IO ()
codeGenMock = do
  code <- withContext genWithContext
  putStrLn code
  return ()

