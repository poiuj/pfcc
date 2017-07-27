{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Syntax

import Control.Monad.State

import Data.Map.Lazy as M

import LLVM.AST as AST
import LLVM.AST.Global
import LLVM.AST.Type

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

types = fromList [
  -- basic classes
  ("String" , ptr i32),
  ("Int", i32),
  ("Bool", i1),
  ("Object", ptr i32),
  ("IO", ptr i32)]

runLLVM :: AST.Module -> (LLVM a) -> AST.Module
runLLVM mod (LLVM m) = execState m mod

addDefinition :: Definition -> LLVM ()
addDefinition definition = do
  definitions <- gets moduleDefinitions
  modify $ \m -> m { moduleDefinitions = definition : definitions }

toLLVMType :: Syntax.Name -> Type
toLLVMType typeName =
  case M.lookup typeName types of
      (Just _type) -> _type
      -- default case is pointer to object
      Nothing -> ptr i32

toLLVMFormal :: Formal -> Parameter
toLLVMFormal (Formal name typeName) = Parameter (toLLVMType typeName) (Name name) []

defun ::  Syntax.Name -> Syntax.Name -> [Formal] -> Syntax.Name -> Expr -> LLVM ()
defun className methodName formals returnTypeName body = addDefinition $
  GlobalDefinition $ functionDefaults {
  name = Name $ className ++ "." ++ methodName
  , parameters = (fmap toLLVMFormal formals, False)
  , returnType = toLLVMType returnTypeName
  }


codegenTop :: Class -> LLVM ()
codegenTop (Class name _ features) = mapM_ (codegenMethod name) features

codegenMethod :: Syntax.Name -> Feature -> LLVM ()
codegenMethod currClassName (Method name formals return body) = defun currClassName name formals return body
codegenMethod _ _ = return ()
