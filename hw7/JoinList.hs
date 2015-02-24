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
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) a b = Append (mappend (tag a) (tag b)) a b

-----------------------------------------
-- exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m a) = if (i == 0) then (Just a) else Nothing
indexJ i (Append m a b) = undefined
