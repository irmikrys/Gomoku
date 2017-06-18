
-------------------------------------

module Board where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import qualified Data.List as List

------------ globals ----------------

myBoard = Board Map.empty

nums = [1,2..9]
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

-- Compares colors when one is maybe a color (used in rate block)
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

northEast :: Position -> Position
northEast (Position row col) = Position (row - 1) (col - 1)

north :: Position -> Position
north (Position row col) = Position (row - 1) col

northWest :: Position -> Position
northWest (Position row col) = Position (row - 1) (col + 1)

west :: Position -> Position
west (Position row col) = Position row (col + 1)

southWest :: Position -> Position
southWest (Position row col) = Position (row + 1) (col + 1)

south :: Position -> Position
south (Position row col) = Position (row + 1) col

southEast :: Position -> Position
southEast (Position row col) = Position (row + 1) (col - 1)

east :: Position -> Position
east (Position row col) = Position row (col - 1)

-- Get point neighbor by direction
getNeighbor :: Position -> Direction -> Position
getNeighbor pos dir = case dir of
  NorthEast -> northEast pos
  North     -> north pos
  NorthWest -> northWest pos
  West      -> west pos
  SouthWest -> southWest pos
  South     -> south pos
  SouthEast -> southEast pos
  East      -> east pos

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

instance Eq Game where
  Game b1 c1 == Game b2 c2 = (b1 == b2) && (c1 == c2)

-------------------------------------

newtype Board = Board(Map.Map Position Color)

instance Show Board where
  show = showBoard

instance Eq Board where
  Board m1 == Board m2 = m1 == m2

-------------------------------------

addToBoard :: Position -> Color -> Board -> Board
addToBoard pos col board@(Board prevMap)
  | checkPos pos board  = Board(Map.insert pos col prevMap)
  | otherwise           = board

-- Returns maybe color of checked position
getPosColor :: Position -> Board -> Maybe Color
getPosColor pos (Board boardMap) = Map.lookup pos boardMap

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
upDownLabel = "  " ++ concat [charToString c  ++ " " | c <- take (last nums) chars] ++ "\n"

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
nextMoves :: Game -> Position -> [Position] -> [Position]
nextMoves (Game board col) pos posList  =
  newPosList where
    updatedPosList = List.delete pos posList
    neighbors = getPointNeighbors pos board
    newPosList = updatedPosList ++ [p | p <- neighbors, p `notElem` updatedPosList, p /= pos]

-- Generates next possible moves
nextGames :: Game -> Position -> [Position] -> [Game]
nextGames game@(Game board col) pos posList =
  [Game (addToBoard p col board) col | p <- nextMoves game pos posList]

---------- rate function ------------

-- Rates board after each move
rate :: Game -> Int
rate (Game board col) = undefined

-- Rates vertically (North to South direction)
rateNS :: Game -> Int
rateNS(Game board col) = undefined

-- Rates horizontally (West to East direction)
rateWE :: Game -> Int
rateWE (Game board col) = undefined

-- Rates diagonally (NorthWest to SouthEast direction)
rateNWSE :: Game -> Int
rateNWSE (Game board col) = undefined

-- Rates diagonally (NorthEast to SouthWest direction)
rateNESW :: Game -> Int
rateNESW (Game board col) = undefined

-- Rates how much is worth each of the following:
-- one with one side open
-- one with both sides open
-- two with one side open
-- two with both sides open
-- three with one side open
-- three with both sides open
-- four with one side open
-- four with two sides open
-- five
-- otherwise the move is not worth anything
value :: Float -> Int
value counter = case counter of
  1 -> 1
  1.5 -> 2
  2 -> 5
  2.5 -> 10
  3 -> 20
  3.5 -> 35
  4 -> 60
  4.5 -> 150
  5 -> 500
  _ -> 0

getKeys :: Board -> [Position]
getKeys (Board boardMap) = Map.keys boardMap

getCurrColPosList :: Game -> [Position]
getCurrColPosList (Game board col) =
  [pos | pos <- getKeys board, compareColors (getPosColor pos board) col]

------------- game tree -------------

infTree = Tree.Node 1 [infTree] -- drzewo nieskonczone

------------- make move -------------

makeMove = undefined

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
  let col2 = changeColor col
  let pos = Position 4 6
  let board = addToBoard pos col scopeBoard
  let game = Game board col2
  let posList = nextMoves game pos []
  print posList
  let gamesList = nextGames game pos []
  print gamesList
  let pos2 = Position 5 7
  let board2 = addToBoard pos2 col2 board
  let game2 = Game board2 col
  let posList2 = nextMoves game2 pos2 posList
  print posList2
  let gamesList2 = nextGames game2 pos2 posList
  print gamesList2
  putStrLn ""
  --askPlayer (Game scopeBoard col)


-- levels (lista do poziomu)
-- Data.Tree.Pretty (rysowanie drzewa)
