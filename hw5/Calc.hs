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

-----------------------------------------
-- Exercise 4

instance Expr Integer where
  lit i = i
  add i1 i2 = i1 + i2
  mul i1 i2 = i1 + i2

instance Expr Bool where
  lit b | b <= 0 = False
        | otherwise = True
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit i = MinMax i
  add (MinMax i) (MinMax i2) = MinMax (max i i2)
  mul (MinMax i) (MinMax i2) = MinMax (min i i2)

instance Expr Mod7 where
  lit i = Mod7 $ i `mod` 7        
  add (Mod7 x) (Mod7 y) = Mod7 ( (x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ( (x * y) `mod` 7)



newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
