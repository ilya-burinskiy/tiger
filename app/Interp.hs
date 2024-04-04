module Interp where

import Ast (Binop (Div, Minus, Plus, Times), Exp (..), Id, Stm (..))
import Data.Map qualified as Map

maxArgs :: Stm -> Int
maxArgs stm =
  let maxArgsHelper (IdExp _) = 0
      maxArgsHelper (NumExp _) = 0
      maxArgsHelper (OpExp exp1 _ exp2) = max (maxArgsHelper exp1) (maxArgsHelper exp2)
      maxArgsHelper (EseqExp stm expr) = max (maxArgs stm) (maxArgsHelper expr)
   in case stm of
        (CompoundStm stm1 stm2) -> max (maxArgs stm1) (maxArgs stm2)
        (AssignStm _ expr) -> maxArgsHelper expr
        (PrintStm exprs) -> foldl max (length exprs) $ map maxArgsHelper exprs

interpStm :: Stm -> Map.Map Id Int -> Map.Map Id Int
interpStm (CompoundStm stm1 stm2) lookup = interpStm stm2 (interpStm stm1 lookup)
interpStm (AssignStm id exp) lookup =
  let (val, lookup') = interpExp exp lookup
   in Map.insert id val lookup'
interpStm (PrintStm exprs) lookup = foldl (\lookup exp -> snd (interpExp exp lookup)) lookup exprs

interpExp :: Exp -> Map.Map Id Int -> (Int, Map.Map Id Int)
interpExp (IdExp id) lookup =
  case Map.lookup id lookup of
    Nothing -> (0, Map.insert id 0 lookup) -- NOTE: maybe throw exception
    Just x -> (x, lookup)
interpExp (NumExp num) lookup = (num, lookup)
interpExp (OpExp lhs op rhs) lookup =
  let (lhsVal, lookup') = interpExp lhs lookup
      (rhsVal, lookup'') = interpExp rhs lookup'
   in case op of
        Plus -> (lhsVal + rhsVal, lookup'')
        Minus -> (lhsVal - rhsVal, lookup'')
        Times -> (lhsVal * rhsVal, lookup'')
        Div -> (lhsVal `div` rhsVal, lookup'') -- NOTE: handle zero division
interpExp (EseqExp stm exp) lookup = interpExp exp (interpStm stm lookup)
