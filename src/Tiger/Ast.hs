module Tiger.Ast where

type TypeId = String

type Id = String

data Prog = Prog [Dec] Expr deriving (Show)

data Dec
  = AliasTypeDec TypeId TypeId
  | RecordTypeDec TypeId [TypeField]
  | ArrayTypeDec TypeId TypeId
  | VarDec Id (Maybe TypeId) Expr
  | FunDec Id [TypeField] (Maybe TypeId) Expr
  deriving (Show, Eq)

data TypeField = TypeField Id TypeId deriving (Show, Eq)

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
  | IfThenElseExpr Expr Expr (Maybe Expr)
  | WhileExpr Expr Expr
  | ForExpr Id Expr Expr Expr
  | BreakExpr
  | LetExpr [Dec] [Expr]
  deriving (Show, Eq)

data Lvalue
  = IdLvalue Id
  | RecordLvalue Lvalue Id
  | ArrayLvalue Lvalue Expr
  deriving (Show, Eq)
