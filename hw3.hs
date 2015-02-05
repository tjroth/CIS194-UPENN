module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips xs = map fns e
  where
    a = zip xs [1..(length xs)]
    b = zip a [1..(length a)]
    c = replicate (length b) b
    fn (n,ar) = filter (\(c,m) -> mod m n == 0) ar
    d = zip [1..length b] c
    e = map fn d
    fns = map(\((c,n),m) -> c)
                

skips2 xs = zip [1..(length xs)] xs

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (a:b:c:xs) | b > a && b > c = b : localMaxima (b:c:xs)
                       | otherwise = localMaxima (b:c:xs)
localMaxima (_:_) = []


histogram :: [Integer] -> String
histogram xs = (concat $ map (\y -> drawFn (vals xs) y) $ reverse [1..9]) ++ "==========\n0123456789\n"
  where
    vals xss = map length $ (map (\y -> filter (\x -> y == x) xs) [0..9])
    drawFn xs y = (concat $ map(\x -> if x >= y then "*" else " ") xs) ++ "\n"

