{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import ExprT
import Parser
import StackVM

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT where
    lit = Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<=0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = lit $ max a b
    mul (MinMax a) (MinMax b) = lit $ min a b

instance Expr Mod7 where
    lit a = Mod7 $ a `mod` 7
    add (Mod7 a) (Mod7 b) = lit $ (a + b)
    mul (Mod7 a) (Mod7 b) = lit $ (a * b)

instance Expr Program where
    lit a = [PushI a]
    add a b = a ++ b ++ [StackVM.Add]
    mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

eval :: ExprT -> Integer
eval expr = case expr of
    Lit n   -> n
    ExprT.Add a b -> eval a + eval b
    ExprT.Mul a b -> eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit ExprT.Add ExprT.Mul

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
