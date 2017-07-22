module Emit where

import Syntax
import Codegen

import LLVM.Module
import LLVM.AST as AST
import LLVM.Context

import Control.Monad.Except

runEmit :: ExceptT String IO String -> IO String
runEmit e = do
  result <- runExceptT e
  case result of
    Right code -> return code
    Left error -> putStrLn error >> return ""

emitInContext :: AST.Module -> Context -> IO String
emitInContext fileModule ctx =
  runEmit $ withModuleFromAST ctx fileModule moduleLLVMAssembly

makeModule :: FilePath -> AST.Module
makeModule filepath =
  defaultModule {
  moduleName = filepath,
  moduleSourceFileName = filepath
  }

emit :: FilePath -> Program -> IO String
emit filepath ast = do
  let fileModule = makeModule filepath
  let finalModule =
        runLLVM fileModule . mapM codegenTop . programClasses $ ast
  withContext $ emitInContext finalModule
