import Data.Char

-- Problem 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n | n < 0 = []
           | otherwise = map (fromIntegral . digitToInt) $ show n

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : (2 * y) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum $ map (sum . toDigits) xs

validate :: Integer -> Bool
validate n | n < 0 = False
           | otherwise = res `mod` 10 == 0
  where
    res = sumDigits . doubleEveryOther . toDigitsRev $ n 

-- Problem 2
