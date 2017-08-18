{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Cons a as) = Cons (fn a) $ streamMap fn as

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = Cons x $ streamFromSeed fn (fn x)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

nats1 :: Stream Integer
nats1 = streamMap (+1) nats

squares :: [(Integer, Integer)]
squares = [ (x, 2^x) | x <- [0..] ]

rulerIndex :: Integer -> Integer
rulerIndex n = fst $ last $ filter ((==) 0 . mod n . snd) $ takeWhile ((<= n) . snd) squares

ruler :: Stream Integer
ruler = streamMap rulerIndex nats1

_interleaveStreams :: Stream a -> Stream a -> Stream a
_interleaveStreams as (Cons b bs) = Cons b (interleaveStreams as bs)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a (_interleaveStreams as bs)

_ruler1 :: Integer -> Stream Integer
_ruler1 n = interleaveStreams (streamRepeat n) (_ruler1 (n + 1))

ruler1 :: Stream Integer
ruler1 = _ruler1 0

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    (+) (Cons a as) (Cons b bs) = Cons (a + b) ((+) as bs)
    negate (Cons a as) = Cons (negate a) (negate as)
    (*) (Cons a as) bbs@(Cons b bs) = Cons (a * b) (fromInteger a * bs + as * bbs)

instance Fractional (Stream Integer) where
    (/) (Cons a as) (Cons b bs) = qs
        where qs = Cons (div a b) ((as - qs * bs) / fromInteger b)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Matrix Integer Integer Integer Integer
    deriving Show

instance Num Matrix where
    fromInteger n = Matrix n n n n
    (+) (Matrix a b c d) (Matrix e f g h) = Matrix (a + e) (b + f) (c + g) (d + h)
    negate (Matrix a b c d) = Matrix (negate a) (negate b) (negate c) (negate d)
    (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)

_b :: Matrix -> Integer
_b (Matrix _ b _ _) = b

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = _b $ (Matrix 1 1 1 0)^n
