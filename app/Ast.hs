module Ast where

data TypeDec = TypeDec TypeId Type deriving (Show)

data Type
  = TypeId TypeId
  | TypeFields [TypeField]
  | ArrayType TypeId
  deriving (Show)

type TypeId = String

type Id = String

data TypeField = TypeField Id TypeId deriving (Show)

data VarDec
  = VarDec Id Expr
  | TypedVarDec Id TypeId Expr
  deriving (Show)

data FunDec
  = FunDec Id [TypeField] Expr
  | TypedFunDec Id [TypeField] TypeId Expr
  deriving (Show)

data Expr
  = IdExpr Id
  | IntExpr Int
  | NegExpr Expr
  | SumExpr Expr Expr
  | SubExpr Expr Expr
  | ProductExpr Expr Expr
  | DivExpr Expr Expr
  | IfThenElseExpr Expr Expr Expr
  | IfThenExpr Expr Expr
  deriving (Show)
