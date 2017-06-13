-- exercise 1

lastDigit :: Integer -> Integer
lastDigit n = mod n 10

shift :: Integer -> Integer
shift n = div n 10

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = (toDigits . shift) n ++ [lastDigit n]

-- exercise 2

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:zs) = x:(2*y):doubleEveryOtherRev zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- exercise 3

flatten :: [[Integer]] -> [Integer]
flatten = foldl (++) []

sumDigits :: [Integer] -> Integer
sumDigits = sum . flatten . (map toDigits)

-- exercise 4

validateSum :: Integer -> Bool
validateSum n = lastDigit n == 0

validate :: Integer -> Bool
validate = validateSum . sumDigits . doubleEveryOther . toDigits
