module Calc where

import ExprT
import Parser


-----------------------------------------
-- Exercise 1

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

-----------------------------------------
-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Nothing -> Nothing
  Just e -> Just $ eval e
