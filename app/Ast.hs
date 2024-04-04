module Ast where

type Id = String

data Binop = Plus | Minus | Times | Div deriving (Show)

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]
  deriving (Show)

data Exp
  = IdExp Id
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp
  deriving (Show)
