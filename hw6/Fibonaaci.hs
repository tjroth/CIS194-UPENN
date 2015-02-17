-------------------------------
--Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0 .. ]

-------------------------------
--Exercise 2
fibs2 = [0, 1] ++ [ fibs2!!(n-1) + fibs2!!(n-2) | n <- [2..] ]

-------------------------------
--Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream v s) = v : (streamToList s)

instance Show a => Show (Stream a) where
  show s = show $ take 50 $ streamToList s

sampleStream a = Stream (a) $ sampleStream (a + 1)

-------------------------------
--Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Stream a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = Stream (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a))

-------------------------------
--Exercise 5
nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = streamMap rFn (streamFromSeed (+1) 1)
  where
    rFn :: Integer -> Integer
    rFn n | n == 0 = 0
          | odd n = 0
          | otherwise = 1 + (rFn (n `div` 2))
