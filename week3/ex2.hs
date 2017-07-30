type Triplet = (Integer, Integer, Integer)

localMaxima :: [Integer] -> [Integer]
localMaxima xs
    | length xs > 2 = findMaxima xs
    | otherwise     = []

findMaxima :: [Integer] -> [Integer]
findMaxima = extractMaxima . filterMaxima . toTriplets

toTriplets :: [Integer] -> [Triplet]
toTriplets xs = zip3 xs (drop 1 xs) (drop 2 xs)

filterMaxima :: [Triplet] -> [Triplet]
filterMaxima = filter (\(a, b, c) -> b > a && b > c)

extractMaxima :: [Triplet] -> [Integer]
extractMaxima = map (\(_, b, _) -> b)
