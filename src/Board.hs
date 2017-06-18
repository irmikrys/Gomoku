-----------------------------------------------

module Board where

import qualified Data.Map.Strict as Map

import Color
import Position

-----------------------------------------------

nums = [1,2..9]
chars = ['A'..'S']

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

-- Returns maybe color of checked position
getPosColor :: Position -> Board -> Maybe Color
getPosColor pos (Board boardMap) = Map.lookup pos boardMap

-- Returns all occupied positions
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

packRow :: Board -> Int -> String
packRow board row = charToString (chars !! (row - 1)) ++ " "
  ++ getStringRow board row ++ charToString(chars !! (row - 1))

getAllStringRows :: Board -> String
getAllStringRows board = concat [packRow board row ++ "\n" | row <- nums]

upDownLabel :: String
upDownLabel = "  " ++ concat [charToString c  ++ " " | c <- take (last nums) chars] ++ "\n"

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

-- Checks if neighbor isn't out of board or occupied,
-- unchecked might be obtained by using getPointNeighbors from "Position.hs"
checkPointNeighbors :: [Position] -> Board -> [Position]
checkPointNeighbors unchecked board = [n | n <- unchecked, checkPos n board]

---------------- helpful ------------------

charToString :: Char -> String
charToString c = [c]
