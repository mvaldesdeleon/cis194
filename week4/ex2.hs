data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert na Leaf = Node 0 Leaf na Leaf
insert na (Node h l a r)
    | height l < height r   = Node h nl a r
    | height l > height r   = Node h l a nr
    | height nl < height nr = Node (height nl + 1) nl a r
    | otherwise             = Node (height nr + 1) l a nr
    where
        nl = insert na l
        nr = insert na r

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l _ r) = (isBalanced l) && (isBalanced r) && (abs (height l - height r) < 2)
