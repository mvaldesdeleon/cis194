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
