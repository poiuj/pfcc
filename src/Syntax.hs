module Syntax where

type Name = String

data Expr
  = Id Name
  | Int Integer
  | BinOp Op Expr Expr
  deriving (Show, Eq)

data Op
  = Plus
  | Minus
  | Mul
  | Div
  deriving (Show, Eq)
