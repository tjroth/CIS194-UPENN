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
  show s = show $ take 10 $ streamToList s

sampleStream a = Stream (a) $ sampleStream (a + 1)
