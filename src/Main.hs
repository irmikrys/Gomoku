-----------------------------------------------

module Main where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import qualified Data.List as List
import Control.Parallel.Strategies
import Control.Monad
import Numeric
import Data.Char
import Text.Read

import Color
import Position
import Board
import Game

---------------- player input -----------------

-- Deprecated
askPlayer :: Game -> IO ()
askPlayer (Game board col) =
  do
    print board
    putStrLn ("Player: " ++ show col ++ ", choose position: ")
    x <- getLine
    y <- getLine
    let col2 = changeColor col
    askPlayer (Game (addToBoard (parseToPosition x y) col board) col2)

-- Deprecated
parseToPosition :: String -> String -> Position
parseToPosition x y =
  Position (read x) (read y)

------------------- parse ---------------------

parsePosition :: String -> Position
parsePosition stringPosition
    | length splittedString == 2 = parse splittedString
    | otherwise = Position 0 0
    where
        splittedString = words stringPosition

parse ::[String] -> Position
parse [row, col]
  | (x > 0) && (x < size + 1) && ((y - 64) > 0) && ((y - 64) < size + 1) = Position x (y - 64)
  | (x > 0) && (x < size + 1) && ((y - 96) > 0) && ((y - 96) < size + 1) = Position x (y - 96)
  | otherwise = Position 0 0
    where
      size = last nums
      x = toInt row
      y = ord $ strToChar col

toInt :: String -> Int
toInt = read

strToChar :: String -> Char
strToChar [ch] = ch
strToChar string = 'Z'

-------------- player vs player ---------------

playerVSplayer :: Game -> IO ()
playerVSplayer (Game board col) =
  if victory (Game board opCol)
    then do
      print (Game board opCol)
      putStrLn ("Player" ++ show opCol ++ " won!")
    else do
      print (Game board col)
      putStrLn "Choose Position (e.g. 6 B): "
      pos <- getLine
      let parsedPos = parsePosition pos
      if checkPos parsedPos board
        then do
          let newBoard = addToBoard parsedPos col board
          playerVSplayer (Game newBoard opCol)
        else do
          putStrLn "Position not available"
          playerVSplayer (Game board col)
          putStr ""
  where
    opCol = changeColor col

------------- computer vs player --------------

computerVSplayer:: Game -> [Position] -> IO ()
computerVSplayer (Game board col) neighborsList =
  if victory (Game board opCol)
    then do
      print (Game board opCol)
      putStrLn ("Computer " ++ show opCol ++ " won!")
    else do
      print (Game board col)
      putStrLn "Choose Position (e.g. 8 A): "
      pos <- getLine
      let parsedPos = parsePosition pos
      if checkPos parsedPos board
        then do
          let newBoard = addToBoard parsedPos col board
          let newGame = Game newBoard col
          print newGame
          let updatedNeighborsList = nextMoves newGame parsedPos neighborsList
          if victory newGame
            then do
              print (Game newBoard col)
              putStrLn ("Player " ++ show col ++ " won!")
            else do
              let boardAndList = getMaxMinBoard (Game newBoard opCol) parsedPos updatedNeighborsList
              computerVSplayer (Game (fst boardAndList) col) (snd boardAndList)
      else do
        putStrLn "Position not available"
        computerVSplayer (Game board col) neighborsList
        putStr ""
  where
    opCol = changeColor col

------------ computer vs computer -------------

botVSbot :: Game -> [Position] -> IO ()
botVSbot (Game board col) positions =
  if victory (Game board opCol)
    then do
      print (Game board opCol)
      putStrLn ("Player" ++ show opCol ++ " won!")
    else do
      let boardAndList = getMaxMinBoardBot (Game board col) positions
      print (Game (fst boardAndList) opCol)
      botVSbot (Game (fst boardAndList) opCol) (snd boardAndList)
  where
    opCol = changeColor col

------------------- main ----------------------

main :: IO ()
main = do
  putStrLn ("\n============== " ++ show White ++ " Gomoku " ++ show Black ++ " ===============")
  let emptyBoard = Board Map.empty
  let board1 = addToBoard (Position 2 2) Black emptyBoard
  --playerVSplayer (Game emptyBoard White)
  --computerVSplayer (Game emptyBoard White) []
  botVSbot (Game board1 White) (possibleMoves board1)
  putStr ""
