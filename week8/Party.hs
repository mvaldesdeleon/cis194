module Party where

import Data.Monoid
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons empl (GL empls fun) = GL (empl:empls) (empFun empl + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emplsa funa) (GL emplsb funb) = GL (emplsa ++ emplsb) (funa + funb)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max
