{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- exercise 1

parseMessageWords :: [String] -> LogMessage
parseMessageWords ("I":t:msg) = LogMessage Info (read t) (unwords msg)
parseMessageWords ("W":t:msg) = LogMessage Warning (read t) (unwords msg)
parseMessageWords ("E":l:t:msg) = LogMessage (Error (read l)) (read t) (unwords msg)
parseMessageWords msg = Unknown (unwords msg)

parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

parse :: String -> [LogMessage]
parse = ((<$>) parseMessage) . lines

-- exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ tm _) (Node lt msgn@(LogMessage _ tn _) rt) = if tm < tn then Node (insert msg lt) msgn rt else Node lt msg (insert msg rt)
insert _ tree = tree

-- exercise 3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt msg rt) = inOrder lt ++ [msg] ++ inOrder rt


-- exercise 5

filterBadErrors :: [LogMessage] -> [LogMessage]
filterBadErrors [] = []
filterBadErrors (x:xs) = case x of
    LogMessage (Error n) _ _ | n > 50 -> [x] ++ filterBadErrors xs
    _ -> filterBadErrors xs

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = ((<$>) getMessage) . inOrder . build . filterBadErrors
