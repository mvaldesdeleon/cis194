import Data.List

type Bucket = (Integer, Integer)

histogram :: [Integer] -> String
histogram = renderHistogram . buildHistogram

buildHistogram :: [Integer] -> [Bucket]
buildHistogram = zip [0..9] . map (subtract 1 . toInteger . length) . group . sort . (++ [0..9])

renderHistogram :: [Bucket] -> String
renderHistogram = unlines . transpose . map reverse . pad . map renderRow

renderRow :: Bucket -> String
renderRow (i, n) = show i ++ renderBar n

renderBar :: Integer -> String
renderBar = ((++) "=") . flip replicate '*' . fromIntegral

pad :: [String] -> [String]
pad xs = realPad (toInteger . maximum $ map length xs) xs

realPad :: Integer -> [String] -> [String]
realPad n = map (padTo n ' ')

padTo :: Integer -> Char -> String -> String
padTo n c s
 | length s < fromIntegral n = s ++ replicate (fromIntegral n - length s) c
 | otherwise    = s
