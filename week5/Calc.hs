import ExprT
import Parser

eval :: ExprT -> Integer
eval expr = case expr of
    Lit n   -> n
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul
