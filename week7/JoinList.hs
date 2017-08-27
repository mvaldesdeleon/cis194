{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

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
          | i > tagSize l - 1              = Empty
dropJ i (Append m l r) | i - 1 < tagSize l = (dropJ i l) +++ r
                       | otherwise         = dropJ (i - tagSize l) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i l | i <= 0                         = Empty
          | i > tagSize l - 1              = l
takeJ i (Append _ l r) | i - 1 < tagSize l = takeJ i l
                       | otherwise         = l +++ takeJ (i - tagSize l) r

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

single :: String -> JoinList (Score, Size) String
single s = Single (scoreString s, Size 1) s

-- provide Buffer instance for JoinList (Score, Size) String
instance Buffer (JoinList (Score, Size) String) where
  -- toString :: b -> String
  toString = unlines . jlToList
  -- fromString :: String -> b
  fromString = foldl (+++) Empty . map single . lines
  -- line :: Int -> b -> Maybe String
  line = indexJ
  -- replaceLine :: Int -> String -> b -> b
  replaceLine n _ b | n < 0           = b
                    | n >= numLines b = b
  replaceLine n l b                   = takeJ n b +++ single l +++ dropJ (succ n) b
  -- numLines :: b -> Int
  numLines = tagSize
  -- value :: b -> Int
  value = getScore . fst . tag

buff :: JoinList (Score, Size) String
buff = fromString . unlines $
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor buff
