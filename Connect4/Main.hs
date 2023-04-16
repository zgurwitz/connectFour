module Main where

import Game
import Util
import Data.Maybe
import Data.List
import Data.Char

main = handleMove defaultBoard 1

-- handle a move
handleMove :: Board -> Player -> IO Board
handleMove (Board b) p = do
  print $ Board b
  let win = winning (Board b)
  if win /= 0 || isDrawn (Board b) then gameOver win else do
    putStrLn $ "\nPlayer " ++ [intToDigit p] ++ " to move:"
    i <- getLine
    if read i == -1 then return $ board 0 0 else do
      let move = place (Board b) p (read i)
      if isNothing move then
        complain (Board b) p else handleMove (fromJust move) (3 - p)

-- handle an invalid move
complain :: Board -> Player -> IO Board
complain b p = do
  print "Invalid move."
  handleMove b p

-- handle a game ending given a winner
gameOver :: Player -> IO Board
gameOver p = do 
  if p == 0 then putStrLn "Tie game, input -1 to play again." else putStrLn $ "Player " ++ [intToDigit p] ++ " wins! Input -1 to play again."
  i <- getLine
  if read i == -1 then handleMove defaultBoard 1 else gameOver 0 

  
