{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Syntax

import Control.Monad.State

import LLVM.AST as AST
import LLVM.AST.Global

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> (LLVM a) -> AST.Module
runLLVM mod (LLVM m) = execState m mod

addDefinition :: Definition -> LLVM ()
addDefinition definition = do
  definitions <- gets moduleDefinitions
  modify $ \m -> m { moduleDefinitions = definition : definitions }

defun :: String -> LLVM ()
defun funcName = addDefinition $
  GlobalDefinition $ functionDefaults {
  name = Name funcName
  , returnType = VoidType
  }


codegenTop :: Feature -> LLVM ()
codegenTop (Method name formals result body) = defun name
