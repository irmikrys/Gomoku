
-------------------------------------

module Board where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree

------------ globals ----------------

myBoard = Board Map.empty

nums = [1,2..19]
chars = ['A'..'S']
currCol = White
directions = [NorthEast, North, NorthWest, West, SouthWest, South, SouthEast, East]

-------------------------------------

data Color = Black | White
  deriving Eq

instance Show Color where
  show Black = "\9787"
  show White = "\9786"

instance Read Color where
  readsPrec _ input
    | head input == 'W' = [(White, tail input)]
    | head input == 'B' = [(Black, tail input)]
    | otherwise = []

changeColor :: Color -> Color
changeColor col
  |col == White   = Black
  |otherwise      = White

compareColors :: Maybe Color -> Color -> Bool
compareColors c1 c2
  |Data.Maybe.isJust c1 = getBare c1 == c2
  |otherwise = False

-------------------------------------

data Direction =
  NorthEast | North | NorthWest | West |
  SouthWest | South | SouthEast | East
    deriving Show

-------------------------------------

data Position = Position Int Int

instance Show Position where
  show (Position x1 x2) = "P(" ++ show x1 ++ "," ++ show x2 ++ ")"

instance Eq Position where
  Position x1 y1 == Position x2 y2 = x1 == x2 && y1 == y2

instance Ord Position where
  compare (Position x1 y1) (Position x2 y2)
    | x1 == x2 && y1 == y2      = EQ
    | x1 > x2                   = GT
    | (x1 == x2 ) && (y1 > y2)  = GT
    | (x1 == x2 ) && (y1 < y2)  = LT
    | x1 < x2                   = LT

-- Get point neighbor by direction
getNeighbor :: Position -> Direction -> Position
getNeighbor (Position row col) dir = case dir of
  NorthEast -> Position (row - 1) (col - 1)
  North     -> Position (row - 1) col
  NorthWest -> Position (row - 1) (col + 1)
  West      -> Position row (col + 1)
  SouthWest -> Position (row + 1) (col + 1)
  South     -> Position (row + 1) col
  SouthEast -> Position (row + 1) (col - 1)
  East      -> Position row (col - 1)

-- Get all neighbors (in each direction from position)
-- checks if neighbor isn't out of board or occupied
getPointNeighbors :: Position -> Board  -> [Position]
getPointNeighbors pos board = neighbors
  where
    unchecked = [getNeighbor pos dir | dir <- directions]
    neighbors = [n | n <- unchecked, checkPos n board]

-------------------------------------

data Game = Game Board Color

instance Show Game where
  show (Game board col) = showBoard board ++ playerLabel col

-------------------------------------

newtype Board = Board(Map.Map Position Color)

instance Show Board where
  show = showBoard

-------------------------------------

addToBoard :: Position -> Color -> Board -> Board
addToBoard pos col board@(Board prevMap)
  | checkPos pos board  = Board(Map.insert pos col prevMap)
  | otherwise           = board

getMapValue :: Position -> Board -> Maybe Color
getMapValue pos (Board b) = Map.lookup pos b

---------- showing board ------------

showBoard :: Board -> String
showBoard board = "\n" ++ upDownLabel ++ getAllStringRows board ++ upDownLabel

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
getAllStringRows board = concat [packRow board row ++ "\n" | row <- nums]

upDownLabel :: String
upDownLabel = "  " ++ concat [charToString c  ++ " " | c <- chars] ++ "\n"

----------- player label ------------

playerLabel :: Color -> String
playerLabel col = "\nPlayer: " ++ show col ++ "\n"

------------ next moves -------------

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

-- Not effective
getAllFreePos :: Board -> [Position]
getAllFreePos board = [Position i j | i <- nums, j <- nums, checkPos (Position i j) board]

-- Not effective
nextPossibleMoves :: Game -> [Game]
nextPossibleMoves (Game board@(Board boardMap) color) =
  [Game (addToBoard pos color board) color | pos <- getAllFreePos board]

-- Position of last added point, list of already added to check positions,
-- actual game state, returns new list with neighbors added
-- New neighbors don't include elems already in posList, current pos and
-- occupied positions (already defined in getPointNeighbors)
nextMoves :: Position -> [Position] -> Game -> [Position]
nextMoves pos posList (Game board col) =
  newPosList where
    neighbors = getPointNeighbors pos board
    newPosList = posList ++ [x | x <- neighbors, x `notElem` posList, x /= pos]

---------- rate function ------------

-- Rates board after each move


------------- game tree -------------

infTree = Tree.Node 1 [infTree] -- drzewo nieskonczone

----------- player input ------------

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

------------- helpful ---------------

getBare :: Maybe t -> t
getBare (Just x) = x

charToString :: Char -> String
charToString c = [c]

-------------- main -----------------

main = do
  putStrLn ("\n============== " ++ show White ++ " Gomoku " ++ show Black ++ " ===============")
  let scopeBoard = Board Map.empty
  let col = White
  askPlayer (Game scopeBoard col)
  --putStrLn ("\n============== " ++ show White ++ " Gomoku " ++ show Black ++ " ===============")
  --print myBoard



-- levels (lista do poziomu)
-- Data.Tree.Pretty (rysowanie drzewa)
