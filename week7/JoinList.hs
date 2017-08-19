import Data.Monoid
import Sized

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

x :: JoinList Size String
x = Empty

y :: JoinList Size String
y = Single (Size 1) "boop"

z :: JoinList Size String
z = Append (Size 3) (Append (Size 2) (Single (Size 1) "best") (Single (Size 1) "juice")) (Single (Size 1) "tomato")

tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0                      = Nothing
indexJ i l | i > tagSize l - 1          = Nothing
indexJ 0 (Single _ a)                   = Just a
indexJ i (Append _ l r) | i < tagSize l = indexJ i l
                        | otherwise     = indexJ (i - tagSize l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i l | i <= 0                         = l
dropJ i l | i > tagSize l - 1              = Empty
dropJ i (Append m l r) | i - 1 < tagSize l = (dropJ i l) +++ r
                       | otherwise         = dropJ (i - tagSize l) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i l | i <= 0                         = Empty
takeJ i l | i > tagSize l - 1              = l
takeJ i (Append _ l r) | i - 1 < tagSize l = takeJ i l
                       | otherwise         = l +++ takeJ (i - tagSize l) r
