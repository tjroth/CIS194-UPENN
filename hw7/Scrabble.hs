{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Monoid
import Data.Char (toUpper)

newtype Score = Score Int
              deriving (Eq, Ord, Show, Num)
                       
instance Monoid Score where
  mempty = Score 0
  mappend = (+)


score :: Char -> Score
score c
    | bigC `elem` ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U' ] = 1
    | bigC `elem` ['D', 'G']                                          = 2
    | bigC `elem` ['C', 'M', 'P']                                     = 3
    | bigC `elem` ['F', 'H', 'V', 'W', 'Y']                           = 4
    | bigC `elem` ['K']                                               = 5
    | bigC `elem` ['J', 'X']                                          = 8
    | bigC `elem` ['Q', 'Z']                                          = 10
    | otherwise                                                       = 0
    where bigC = toUpper c
          
scoreString :: String -> Score
scoreString = sum . map score

scoreString' :: String -> Score
scoreString' s = foldl mappend mempty (map score s)
