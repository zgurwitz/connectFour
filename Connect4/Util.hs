module Util where

import Data.Char
import Data.List

getAt :: [a] -> Int -> Maybe a
getAt (x:xs) 0 = Just x
getAt (x:xs) n = getAt xs (n-1)
getAt _ _ = Nothing

setAt :: [a] -> Int -> a -> [a]
setAt (x:xs) n y | n < 0 = x:xs
                 | n == 0 = y:xs
                 | otherwise = x:setAt xs (n-1) y
setAt _ _ _ = []

natural :: [Int]
natural = [0..]

-- a string of the first n digits
firstn :: Int -> String
firstn n = concat (take n (map (singleton . intToDigit) natural))

-- get the long diagonal
diag :: [[a]] -> [a]
diag ((x:xs):xss) = x : diag (map tail xss)
diag _ = []

-- get all diagonals
diags :: [[a]] -> [[a]]
diags xss = f xss ++ f (map reverse xss) where
  f (a:as) = takeWhile (not . null) (map diag $ iterate (\l -> if null l then [] else map tail l) (a:as)) ++ map diag (tails as)
  f [] = []
