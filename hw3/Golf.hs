module Golf where

skips:: [a] -> [[a]]
skips xs = map fn $ map (\num -> filter (\(n,l) -> mod n num == 0) (mrkDig xs)) [1..(length $ mrkDig xs)]
  where
    mrkDig xs = zip [1..(length xs)] xs
    fn xs = map (\(d,v) -> v) xs

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

