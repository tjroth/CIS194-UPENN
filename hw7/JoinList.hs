import Data.Monoid
import Sized


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
