{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = toMessage $ words s
  where
    toMessage ("E":c:t:st) = LogMessage (Error (read c :: Int)) (read t :: Int) (unwords st)
    toMessage ("I":t:st) = LogMessage Info (read t :: Int) (unwords st)
    toMessage ("W":t:st) = LogMessage Warning (read t :: Int) (unwords st)
    toMessage _ = Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tr = tr
insert _ tr@(Node _ (Unknown _) _) = tr
insert lm@(LogMessage _ t _ ) (Node l nm@(LogMessage _ t2 _) r) | t < t2 = Node (insert lm l) nm r
                                                                | otherwise = Node l nm (insert lm r)
insert lm Leaf = Node Leaf lm Leaf

build :: [LogMessage] -> MessageTree
build (x:xs) = foldr insert (Node Leaf x Leaf) xs
build [] = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = (inOrder l) ++  [lm] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map onlyText . filter isCriticalError . inOrder . build 
  where
    isCriticalError (LogMessage (Error n) _ _) | n >= 50 = True
                                               | otherwise = False
    isCriticalError _ = False
    onlyText (LogMessage _ _ s) = s
    onlyText _ = ""
