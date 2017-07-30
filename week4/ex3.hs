xor :: [Bool] -> Bool
xor = odd . length . filter id

xor_ :: Bool -> Bool -> Bool
xor_ True False = True
xor_ False True = True
xor_ _ _        = False

xor' :: [Bool] -> Bool
xor' = foldr xor_ False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

-- Builds a function, then calls it with the initial value
-- The function does the current step, then calls "the next step"
-- This way, the order is reversed
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' f b xs = foldr (\a fn b -> fn $ f b a) id xs $ b
