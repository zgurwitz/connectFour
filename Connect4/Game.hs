{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapM" #-}

module Game where

import Util
import Data.List


type Player = Int
newtype Board = Board [[Int]]

instance Show Board where
  show (Board b) = "\n" ++ concatMap (flip (++) "\n" . concatMap g) b ++ "\n" ++ firstn (length b + 1) ++ "\n" where
    g 1 = ['X']
    g 2 = ['O']
    g _ = ['-']

defaultBoard :: Board
defaultBoard = board 6 7

board :: Int -> Int -> Board
board x = Board . f x where
  f x y = replicate x $ replicate y 0

height :: Board -> Int
height (Board b) = length $ map head b

width :: Board -> Int
width (Board b) = length b

place :: Board -> Player -> Int -> Maybe Board
place (Board b) p x = do
  col <- sequence $ map (`getAt` x) b
  let rown = length (takeWhile ( == 0) col) - 1
  row <- getAt b rown
  return $ Board $ setAt b rown $ setAt row x p

-- 0 is no player, 1 is player 1, 2 is player 2
winning :: Board -> Player
winning (Board b) = if null j then 0 else head j where
  j = filter (/= 0) $ map fourInARow $ b ++ transpose b ++ diags b

isDrawn :: Board -> Bool
isDrawn (Board b) = not $ foldr (\x y -> elem 0 x || y) False b

-- given a list, find the first non-0 4-in-a-row or 0 if none
fourInARow :: [Player] -> Player
fourInARow (a:b:c:d:es) | a /= 0 && a == b && b == c && c == d = a
                        | otherwise = fourInARow (b:c:d:es)
fourInARow _ = 0



