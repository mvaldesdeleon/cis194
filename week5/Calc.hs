import ExprT
import Parser

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval expr = case expr of
    Lit n   -> n
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
