type Peg = String
type Move = (Peg, Peg)

-- exercise 5

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoiMoves :: Integer -> Integer
hanoiMoves n = (fromIntegral . length) (hanoi n "a" "b" "c")

-- exercise 6

topHalf :: Integer -> Integer
topHalf n = div n 2 + mod n 2

bottomHalf :: Integer -> Integer
bottomHalf n = div n 2

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = hanoi4 (topHalf (n - 1)) a d b c ++ hanoi (bottomHalf (n - 1)) a c b ++ [(a, b)] ++ hanoi (bottomHalf (n - 1)) c b a ++ hanoi4 (topHalf (n - 1)) d b a c

hanoi4Moves :: Integer -> Integer
hanoi4Moves n = (fromIntegral . length) (hanoi4 n "a" "b" "c" "d")
