-----------------------------------------------

module Main where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import qualified Data.List as List
import Control.Parallel.Strategies
import Control.Monad( when )
import Numeric ( showHex, showIntAtBase )
import Data.Char
import Text.Read

import Color
import Position
import Board
import Game

---------------- player input -----------------

askPlayer :: Game -> IO ()
askPlayer (Game board col) =
  do
    print board
    putStrLn ("Player: " ++ show col ++ ", choose position: ")
    x <- getLine
    y <- getLine
    let col2 = changeColor col
    askPlayer (Game (addToBoard (parseToPosition x y) col board) col2)

parseToPosition :: String -> String -> Position
parseToPosition x y =
  Position (read x) (read y)

------------------- parse ---------------------

parsePosition :: String -> Position
parsePosition stringWithPosition
    | length splittedString == 2 = parse splittedString
    | otherwise = Position 0 0
    where
        splittedString = words stringWithPosition

parse ::[String] -> Position
parse [row, col]
  | (x > 0) && (x < size + 1) && ((y - 64) > 0) && ((y - 64) < size + 1) = Position x (y - 64)
  | (x > 0) && (x < size + 1) && ((y - 96) > 0) && ((y-96) < size + 1) = Position x (y - 96)
  | otherwise = Position 9 9
    where
      size = last nums
      x = toInt row
      y = ord $ strToChar col

toInt :: String -> Int
toInt = read

strToChar :: String -> Char
strToChar [letter] = letter
strToChar string = 'Z'

------------------- main ----------------------

gameLoop :: Game -> IO ()
gameLoop (Game board col) =
    if victory (Game board opCol)
        then
            putStrLn "Player2 won (computer)"
        else do
            print (Game board col)
            putStrLn "Choose Position (e.g. 6 B): "
            pos <- getLine
            let parsedPos = parsePosition pos
            if checkPos parsedPos board
                then do
                    let newBoard = addToBoard parsedPos col board
                    if victory (Game newBoard col)
                        then do
                          print (Game newBoard col)
                          putStrLn "Player1 won"
                        else do
                          print (Game newBoard opCol)
                          putStrLn "Choose Position (e.g. 4 F): "
                          pos2 <- getLine
                          let parsedPos2 = parsePosition pos2
                          if checkPos parsedPos2 newBoard
                              then do
                                  let newBoard2 = addToBoard parsedPos opCol newBoard
                                  gameLoop (Game newBoard2 opCol)
                                  --let boardAfterMinmax = boardFromMinmax newBoard Black
                                  --gameLoop boardAfterMinmax
                          else do
                              putStrLn "Position not available"
                              gameLoop (Game newBoard col)
                              putStr ""
                else do
                    putStrLn "Position not available"
                    gameLoop (Game board col)
                    putStr ""
  where
    opCol = changeColor col

main :: IO ()
main = do
  let emptyBoard = Board Map.empty
  gameLoop (Game emptyBoard White)
  putStr ""
