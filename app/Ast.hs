module Ast where

type TypeId = String

type Id = String

data Dec
  = AliasTypeDec TypeId TypeId
  | RecordTypeDec TypeId [TypeField]
  | ArrayTypeDec TypeId TypeId
  | VarDec Id Expr
  | TypedVarDec Id TypeId Expr
  | FunDec Id [TypeField] Expr
  | TypedFunDec Id [TypeField] TypeId Expr
  deriving (Show)

data TypeField = TypeField Id TypeId deriving (Show)

data Expr
  = StringExpr String
  | IntExpr Int
  | NilExpr
  | LvalExpr Lvalue
  | NegExpr Expr
  | ProductExpr Expr Expr
  | DivExpr Expr Expr
  | SumExpr Expr Expr
  | SubExpr Expr Expr
  | EqExpr Expr Expr
  | NotEqExpr Expr Expr
  | GtExpr Expr Expr
  | GtOrEqExpr Expr Expr
  | LtExpr Expr Expr
  | LtOrEqExpr Expr Expr
  | AndExpr Expr Expr
  | OrExpr Expr Expr
  | AssignExpr Lvalue Expr
  | CallExpr Id [Expr]
  | ExprSeq [Expr]
  | RecordInstanceExpr TypeId [(Id, Expr)]
  | ArrayInstanceExpr TypeId Expr Expr
  | IfThenElseExpr Expr Expr Expr
  | IfThenExpr Expr Expr
  | WhileExpr Expr Expr
  | ForExpr Id Expr Expr Expr
  | BreakExpr
  | LetExpr [Dec] [Expr]
  deriving (Show)

data Lvalue
  = IdLvalue Id
  | RecordLvalue Lvalue Id
  | ArrayLvalue Lvalue Expr
  deriving (Show)
