--(.) :: (b->c) -> (a->b) -> (a->c)
-- f . g = \x -> f (g x)
--(f . g) x = f (g x)

--curry/uncurry - bierze zamiast krotki argumenty/ zamiast zrgumentow krotke
--implementacja curry/uncarry
-- c f x y = f (x,y)
-------------------------------------

module Board where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree

-------------------------------------

myBoard = Board(Map.empty)
--newBoard = addToBoard (Position 3 4) Black myBoard
--newBoard2 = addToBoard (Position 10 13) White newBoard

nums = [1,2..19]
chars = ['A'..'S']
currCol = White

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

instance Show Board where
  show (Board map) = showBoard (Board map)

addToBoard :: Position -> Color -> Board -> Board
addToBoard pos col (Board prevMap) = Board(Map.insert pos col prevMap)

checkPosRange :: Position -> Bool
checkPosRange (Position x y)
  | x > 0 && x < 20 && y > 0 && y < 20  = True
  | otherwise                           = False

checkPosAvailable :: Position -> Board -> Bool
checkPosAvailable pos (Board boardMap)
  | Map.notMember pos boardMap  = True
  | otherwise                   = False

checkPos :: Position -> Board -> Bool
checkPos pos board = checkPosRange pos && checkPosAvailable pos board

getAllAvailablePos :: Board -> [Position]
getAllAvailablePos = undefined

getFromPlayer :: Color -> Board -> IO ()
getFromPlayer col board = do
  putStrLn ("Player: " ++ show col ++ ", choose position: ")
  x <- getLine
  y <- getLine
  let pos = Position (read x) (read y)
  putStrLn (show pos)
  --if checkPosRange pos then addToBoard pos col board else board

-------------------------------------

showBoard :: Board -> String
showBoard board = "\n" ++ upDownLabel ++ getAllStringRows board ++ upDownLabel ++ playerLabel currCol

showDisc :: Board -> Position -> String
showDisc (Board boardMap) pos
  |Map.member pos boardMap      = show (getBare(Map.lookup pos boardMap)) ++ " "
  |otherwise                    = "- "

getStringRow :: Board -> Int -> String
getStringRow board row = concat [showDisc board (Position row col) | col <- nums]

packRow :: Board -> Int -> String
packRow board row = charToString (chars !! (row - 1)) ++ " "
  ++ getStringRow board row ++ charToString(chars !! (row - 1))

getAllStringRows :: Board -> String
getAllStringRows board = concat [(packRow board row) ++ "\n" | row <- nums]

upDownLabel :: String
upDownLabel = "  " ++ concat [charToString c  ++ " " | c <- chars] ++ "\n"

playerLabel :: Color -> String
playerLabel col = "\nPlayer: " ++ show col ++ "\n"

-------------------------------------

getBare :: Maybe t -> t
getBare (Just x) = x

charToString :: Char -> String
charToString c = [c]

-------------------------------------

main = do
  putStrLn ("\n============== " ++ show White ++ " Gomoku " ++ show Black ++ " ===============")
  putStrLn (showBoard myBoard)


-- levels (lista do poziomu)