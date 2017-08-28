{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Syntax

import Control.Monad.State

import Data.Map.Lazy as M

import LLVM.AST as AST
import LLVM.AST.Constant as Const
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.Instruction as Instruction
import LLVM.AST.Operand

defaultAlignment = 32

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)


type NamesMap = M.Map Syntax.Name Operand

data CodeGenState = CodeGenState {
  namesMap :: NamesMap
  , instructions :: [Named Instruction]
  , valueIndex :: Word
  }
emptyCodeGenState = CodeGenState M.empty [] 0

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

getNextValueName :: CodeGen AST.Name
getNextValueName = do
  currValueIndex <- gets valueIndex
  let nextValueIndex = currValueIndex + 1
  modify $ \s -> s { valueIndex = nextValueIndex }
  return $ UnName nextValueIndex

getPointerType :: Type -> Type
getPointerType (PointerType referentType _) = referentType
getPointerType _ = error "Internal error. Can't get pointer referent's type from a non-pointer type"

load :: Operand -> CodeGen Operand
load addr@(LocalReference addrType _) = do
  valueName <- getNextValueName
  let loadInstruction = valueName := Load False addr Nothing 32 []
  instructionStack <- gets instructions
  modify $ \s -> s { instructions = instructionStack ++ [loadInstruction] }
  return $ LocalReference (getPointerType addrType) valueName

-- expressions
cgen :: Expr -> CodeGen Operand
cgen (Syntax.Int i) = return $ ConstantOperand $ Const.Int 32 i

cgen (BoolConst bool) = return $ ConstantOperand $ Const.Int 1 $
  if bool then 1 else 0

cgen (Id name) = do
  names <- gets namesMap
  let (Just varAddr) = M.lookup name names
  load varAddr

cgen (BinExpr Plus e1 e2) = do
  op1 <- cgen e1
  op2 <- cgen e2
  resultName <- getNextValueName
  let addInstruction = resultName := Instruction.Add False False op1 op2 []
  instructionStack <- gets instructions
  modify $ \s -> s { instructions = instructionStack ++ [addInstruction] }
  -- TODO: Move knowledge about type of Plus expressions somewhere.
  -- It would help in case of adding the float and double types
  return $ LocalReference i32 resultName

-- TODO: Refactoring is needed. All BinExpr expressions are the same.
cgen (BinExpr Minus e1 e2) = do
  op1 <- cgen e1
  op2 <- cgen e2
  resultName <- getNextValueName
  let minusInstruction = resultName := Instruction.Sub False False op1 op2 []
  instructionStack <- gets instructions
  modify $ \s -> s { instructions = instructionStack ++ [minusInstruction] }
  return $ LocalReference i32 resultName

cgen (BinExpr Syntax.Mul e1 e2) = do
  op1 <- cgen e1
  op2 <- cgen e2
  resultName <- getNextValueName
  let mulInstruction = resultName := Instruction.Mul False False op1 op2 []
  instructionStack <- gets instructions
  modify $ \s -> s { instructions = instructionStack ++ [mulInstruction] }
  return $ LocalReference i32 resultName

cgen (BinExpr Div e1 e2) = do
  op1 <- cgen e1
  op2 <- cgen e2
  resultName <- getNextValueName
  let divInstruction = resultName := Instruction.SDiv False op1 op2 []
  instructionStack <- gets instructions
  modify $ \s -> s { instructions = instructionStack ++ [divInstruction] }
  return $ LocalReference i32 resultName


cgen _ = return $ ConstantOperand $ Const.Int 32 0

genActual :: Formal -> CodeGen ()
genActual (Formal name typeName) = do
  actualAddr <- getNextValueName
  let formalLLVMType = toLLVMType typeName
  let allocaInstruction =
        actualAddr := Alloca formalLLVMType Nothing defaultAlignment []
  let actualAddrOperand = LocalReference formalLLVMType actualAddr
  let storeInstruction =
        Do $ Store False actualAddrOperand (LocalReference formalLLVMType (Name name)) Nothing defaultAlignment []
  instructionStack <- gets instructions
  names <- gets namesMap
  modify $ \s -> s { instructions = instructionStack ++ allocaInstruction : [storeInstruction]
                   , namesMap = M.insert name actualAddrOperand names }

genBody :: [Formal] -> Expr -> [BasicBlock]
genBody formals expr =
  let codegen = mapM genActual formals >> cgen expr
      (value, state) = runState (runCodeGen codegen) emptyCodeGenState in
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
