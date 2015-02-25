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
