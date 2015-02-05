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


-----------------------------------------
-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit i = Lit i
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

reify :: ExprT -> ExprT
reify = id
