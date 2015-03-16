{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
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


-----------------------------------------
-- exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (mappend (tag a) (tag b)) a b

-----------------------------------------
-- exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m a) = if (i == 0) then (Just a) else Nothing
indexJ i (Append m a b) | i < sizeOf a = indexJ i a
                        | i == sizeOf a || i > sizeOf a = indexJ (i - (sizeOf a)) b
  where sizeOf = getSize . size . tag

sampleJ :: JoinList Size String
sampleJ = (Append (Size 4)
                ( Append (Size 2) 
                    (Single (Size 1) "Haskell")
                    (Single (Size 1) "Curry")
                )
                ( Append (Size 2) 
                    (Single (Size 1) "Monad")
                    (Single (Size 1) "Monoid")
                )
           )


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl@(Single m a) = jl
dropJ i jl@(Single m a) = Empty
dropJ i (Append m a b) | i == (getSize . size $ m) || i > (getSize . size $ m)  = Empty
                       | otherwise = case compare i (sizeOf a) of
                           GT -> dropJ (i - (sizeOf a)) b
                           EQ -> dropJ (i - (sizeOf a)) b
                           LT -> Append (( tag (dropJ i a)) <> (tag b)) (dropJ i a) b
  where
    sizeOf = getSize . size . tag


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single m a) = if i == 0 then Empty else jl
takeJ i jl@(Append m a b) | i == sizeM m || i > sizeM m = jl
                          | otherwise = case compare i (sizeT a) of
                            LT -> takeJ i a
                            EQ -> takeJ i a
                            GT -> Append ((tag a) <> (tag (takeJ ((sizeT b)-i) b))) a (takeJ ((sizeT b) -i) b)
  where
    sizeM = getSize . size
    sizeT = sizeM . tag


-----------------------------------------
-- exercise 3
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-----------------------------------------
-- exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString jl = show jl
  fromString s = foldr (+++) Empty (map (\s -> (Single (scoreString s, Size 1) s) ) (lines s))
  line n b = indexJ n b
  replaceLine n s b = (takeJ n b) +++ (Single (scoreString s, Size 1) s) +++ (dropJ (n+1) b)
  numLines = getSize . size . tag
  value x = getSize (size (tag x))


main = runEditor editor $ (Single (scoreString s, Size 1) s)
  where
    s = "Oh my, haskell is awesome!!!"
  
