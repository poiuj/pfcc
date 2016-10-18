module Syntax where

type Name = String

data Program = Program [Class] deriving (Show)

data Class = Class {
  className :: Name
  , classbase :: Name
  , classFeatures :: [Feature]
  } deriving (Show)

data Feature =
  Method {
    methodName :: Name
    , methodFormals :: [Formal]
    , methodResult :: Name
    , methodBody :: Expr
    } deriving (Show)

data Formal = Formal {
  formalName :: Name
  , formalType :: Name
  } deriving (Show)

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
