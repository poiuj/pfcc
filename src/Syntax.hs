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
  | BoolConst Bool
  | StringConst String
  | UnExpr UnaryOp Expr
  | BinExpr BinaryOp Expr Expr
  | Assignment Name Expr
  | New Name
  | Compound [Expr]
  | Call Expr Name [Expr]
  | StaticCall Expr Name Name [Expr]
  deriving (Show, Eq)

data BinaryOp
  = Mul | Div
  | Plus | Minus
  | Le | Lt | Eq
  deriving (Show, Eq)

data UnaryOp = Not | Complement | IsVoid deriving (Show, Eq)
