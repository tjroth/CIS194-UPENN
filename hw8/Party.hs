module Party where

import Employee
import Data.Monoid

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp n f) (GL es gf) = GL (es ++ [e]) (gf + f)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) | f1 > f2 = g1
                                  | otherwise = g2
