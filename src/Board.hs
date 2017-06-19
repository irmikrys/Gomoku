-----------------------------------------------

module Board where

import qualified Data.Map.Strict as Map

import Color
import Position

-----------------------------------------------

nums = [1,2..19] -- last number defines size of board
chars = ['A'..'Z']

-----------------------------------------------

newtype Board = Board(Map.Map Position Color)

instance Show Board where
  show = showBoard

instance Eq Board where
  Board m1 == Board m2 = m1 == m2

-----------------------------------------------

addToBoard :: Position -> Color -> Board -> Board
addToBoard pos col board@(Board prevMap)
  | checkPos pos board  = Board(Map.insert pos col prevMap)
  | otherwise           = board

getPosColor :: Position -> Board -> Maybe Color
getPosColor pos (Board boardMap) = Map.lookup pos boardMap

getKeys :: Board -> [Position]
getKeys (Board boardMap) = Map.keys boardMap

--------------- showing board -----------------

showBoard :: Board -> String
showBoard board = "\n" ++ upDownLabel ++ getAllStringRows board ++ upDownLabel

showDisc :: Board -> Position -> String
showDisc (Board boardMap) pos
  |Map.member pos boardMap      = show (unJust(Map.lookup pos boardMap)) ++ " "
  |otherwise                    = "- "

getStringRow :: Board -> Int -> String
getStringRow board row = concat [showDisc board (Position row col) | col <- nums]

packRowChars :: Board -> Int -> String
packRowChars board row = charToStr (chars !! (row - 1)) ++ " "
  ++ getStringRow board row ++ charToStr(chars !! (row - 1))

packRowNums :: Board -> Int -> String
packRowNums board row = leftSideLabelPack (nums !! (row - 1)) ++ " "
  ++ getStringRow board row ++ show (nums !! (row - 1))

getAllStringRows :: Board -> String
getAllStringRows board = concat [packRowNums board row ++ "\n" | row <- nums]

leftSideLabelPack :: Int -> String
leftSideLabelPack num
  | (num > 0) && (num < 10) = " " ++ show num
  | otherwise = show num

upDownLabel :: String
upDownLabel = "   " ++ concat [charToStr c  ++ " " | c <- take (last nums) chars] ++ "\n"

---------- check position within board --------

checkPosRange :: Position -> Bool
checkPosRange (Position x y)
  | x > 0 && x < (last nums + 1) && y > 0 && y < (last nums + 1)  = True
  | otherwise                           = False

checkPosAvailable :: Position -> Board -> Bool
checkPosAvailable pos (Board boardMap)
  | Map.notMember pos boardMap  = True
  | otherwise                   = False

checkPos :: Position -> Board -> Bool
checkPos pos board = checkPosRange pos && checkPosAvailable pos board

--------- check neighbors within board --------

checkPointNeighbors :: Position -> Board -> [Position]
checkPointNeighbors pos board = [n | n <- getPointNeighbors pos, checkPos n board]

---------------- helpful ------------------

charToStr :: Char -> String
charToStr c = [c]
