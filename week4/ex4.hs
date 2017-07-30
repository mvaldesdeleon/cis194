import Data.List

-- Given an integer n, generate all the odd prime numbers up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\n -> 2*n+1) $ [1..n] \\ remove n

remove :: Integer -> [Integer]
remove n = filter (<= n) $ map (\(i,j) -> i+j+2*i*j) $ filter (\(i,j) -> i <= j) $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
