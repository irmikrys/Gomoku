-------------------------------------

module Board where

import Data.Maybe
import qualified Data.Map.Strict as Map

-------------------------------------

data Color =
  Black
  | White

instance Show Color where
  show Black = "\9787"
  show White = "\9786"

-------------------------------------

data Position = Position Int Int

instance Show Position where
  show (Position x1 x2) = "Pos(" ++ show x1 ++ ", " ++ show x2 ++ ")"

instance Eq Position where
  Position x1 y1 == Position x2 y2 = x1 == x2 && y1 == y2

instance Ord Position where
  compare (Position x1 y1) (Position x2 y2)
    | x1 == x2 && y1 == y2      = EQ
    | x1 > x2                   = GT
    | (x1 == x2 ) && (y1 > y2)  = GT
    | (x1 == x2 ) && (y1 < y2)  = LT
    | x1 < x2                   = LT

-------------------------------------

data Board = Board(Map.Map Position Color)

getBare (Just x) = x

showDisc :: Board -> Position -> String
showDisc (Board boardMap) pos
  |Map.member pos boardMap      = show (getBare(Map.lookup pos boardMap))
  |otherwise                    = "."

getStringRow :: Board -> Int -> String
getStringRow board row = rowString
  where
    rowString = concat [(showDisc board (Position row col) ++ " ") | col <- [1,2..19]]

getAllStringRows :: Board -> String
getAllStringRows board = allRowsString
  where
    allRowsString = concat [(getStringRow board row) ++ "\n" | row <- [1,2..19]]

showBoard :: Board -> String
showBoard board = result
  where
    result = getAllStringRows board

addToBoard :: Board -> Position -> Color -> Board
addToBoard (Board prevMap) pos col = Board(Map.insert pos col prevMap)

-------------------------------------

myBoard = Board(Map.empty)
newBoard = addToBoard myBoard (Position 3 4) Black
newBoard2 = addToBoard newBoard (Position 10 13) White

-------------------------------------

main = do
  putStrLn "Gomoku"
  putStrLn (show Black)
  putStrLn (show White)
  putStrLn (showBoard newBoard2)
