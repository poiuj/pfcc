{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Monad.State

import LLVM.AST

newtype LLVM a = LLVM (State Module a)
  deriving (Functor, Applicative, Monad, MonadState Module)