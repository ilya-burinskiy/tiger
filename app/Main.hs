module Main where

import Ast
  ( Binop (Minus, Plus, Times),
    Exp (EseqExp, IdExp, NumExp, OpExp),
    Stm (AssignStm, CompoundStm, PrintStm),
  )
import Data.Map qualified as Map
import Interp (interpStm)

main :: IO ()
main = do
  let prog =
        CompoundStm
          (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
          ( CompoundStm
              ( AssignStm
                  "b"
                  ( EseqExp
                      (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                      (OpExp (NumExp 10) Times (IdExp "a"))
                  )
              )
              (PrintStm [IdExp "b"])
          )
  print $ interpStm prog Map.empty
