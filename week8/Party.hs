module Party where

import Data.Monoid
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons empl (GL empls fun) = GL (empl:empls) (empFun empl + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emplsa funa) (GL emplsb funb) = GL (emplsa ++ emplsb) (funa + funb)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (b -> a -> b) -> (b -> b -> b) -> b -> Tree a -> b
treeFold f g b (Node a ns) = f (foldl g b . map (treeFold f g b) $ ns) a

treeFold' :: (a -> [b] -> b) -> Tree a -> b
treeFold' f (Node a ns) = f a (map (treeFold' f) ns)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss subtrees = (glCons boss (mconcat . map snd $ subtrees), mconcat . map fst $ subtrees)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold' nextLevel
