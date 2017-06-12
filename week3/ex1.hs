oneToLen :: [a] -> [Int]
oneToLen = enumFromTo 1 . length

double :: [Int] -> [Int]
double = map (*2)

nOfN :: Int -> [Int]
nOfN x = replicate x x

target :: [a] -> [Int]
target = nOfN . length

enumFromThenToIndex :: Int -> Int -> Int -> [Int]
enumFromThenToIndex a b c = map pred (enumFromThenTo a b c)

indexSkips :: [a] -> [[Int]]
indexSkips xs = zipWith3 (enumFromThenToIndex) (oneToLen xs) (double . oneToLen $ xs) (target xs)

indexOf :: [a] -> [Int] -> [a]
indexOf xs = map ((!!) xs)

skips :: [a] -> [[a]]
skips xs = map (indexOf xs) (indexSkips xs)
