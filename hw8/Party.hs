module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp n f) (GL es gf) = GL (es ++ [e]) (gf + f)
