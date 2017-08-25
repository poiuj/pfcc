{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Syntax

import Control.Monad.State

import Data.Map.Lazy as M

import LLVM.AST as AST
import LLVM.AST.Constant as Const
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Instruction
import LLVM.AST.Operand

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)


type NamesMap = M.Map Syntax.Name Operand

data CodeGenState = CodeGenState {
  namesMap :: NamesMap
  , instructions :: [Named Instruction]
  }
emptyCodeGenState = CodeGenState M.empty []

newtype CodeGen a = CodeGen { runCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)

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

self :: Parameter
self = Parameter (ptr i32) (Name "self") []

genActuals :: [Formal] -> [Named Instruction]
genActuals formals = []

-- expressions
cgen :: Expr -> CodeGen Operand
cgen (Syntax.Int i) = return $ ConstantOperand $ Const.Int 32 i

cgen (BoolConst bool) = return $ ConstantOperand $ Const.Int 1 $
  if bool then 1 else 0

cgen _ = return $ ConstantOperand $ Const.Int 32 0

genBody :: [Formal] -> Expr -> [BasicBlock]
genBody formals expr =
  let (value, state) = runState (runCodeGen $ cgen expr) emptyCodeGenState in
    [BasicBlock (Name "entry")
      (instructions state)
      (Do $ Ret (Just value) [])]

defun ::  Syntax.Name -> Syntax.Name -> [Formal] -> Syntax.Name -> Expr -> LLVM ()
defun className methodName formals returnTypeName body = addDefinition $
  GlobalDefinition $ functionDefaults {
  name = Name $ className ++ "." ++ methodName
  , parameters = (self : (fmap toLLVMFormal formals), False)
  , returnType = toLLVMType returnTypeName
  , basicBlocks = genBody formals body
  }


codegenTop :: Class -> LLVM ()
codegenTop (Class name _ features) = mapM_ (codegenMethod name) features

codegenMethod :: Syntax.Name -> Feature -> LLVM ()
codegenMethod currClassName (Method name formals return body) = defun currClassName name formals return body
codegenMethod _ _ = return ()
