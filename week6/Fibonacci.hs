fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ [ fibs2!!n + fibs2!!(n+1) | n <- [0..] ]

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) = [a] ++ streamToList as

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList
