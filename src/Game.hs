-----------------------------------------------

module Game where

import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import qualified Data.List as List
import Control.Parallel.Strategies

import Color
import Position
import Board

-----------------------------------------------

rateDirections = [West, SouthWest, South, SouthEast]
numToWin = 3 -- you can change number of discs in-a-row needed to win (2-5)

-----------------------------------------------

data Game = Game Board Color

instance Show Game where
  show (Game board col) = showBoard board ++ playerLabel col board

instance Eq Game where
  Game b1 c1 == Game b2 c2 = (b1 == b2) && (c1 == c2)

---------------- player label -----------------

playerLabel :: Color -> Board -> String
playerLabel col board = "\nPlayer: " ++ show col ++ " rate: " ++ "\n"

----------------- next moves ------------------

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
    neighborsUnchecked = getPointNeighbors pos
    neighbors = checkPointNeighbors neighborsUnchecked board
    newPosList = updatedPosList ++ [p | p <- neighbors, p `notElem` updatedPosList, p /= pos]

-- Generates next possible moves
nextGames :: Game -> Position -> [Position] -> [Game]
nextGames game@(Game board col) pos posList =
  [Game (addToBoard p col board) col | p <- nextMoves game pos posList]

---------------- rate function ----------------

-- Rates board after each move
-- Sums scores of sequences in each direction
rate :: Game -> Int
rate game@(Game board col) = result
  where
    colList = getCurrColPosList game -- tu wszystkie pozycje bieżącego koloru

    result = 1

rateDir :: Game -> Direction -> [Position] -> Int
rateDir game dir colList = undefined

-----------------

rateDirection:: Position -> [Position] -> Int -> Direction -> [Int]
rateDirection _ [] num dir = [num]
rateDirection pos (pos2 : rest) num dir
    | pos `elem` (pos2 : rest) = rateDirection (getNeighbor pos dir) (List.delete pos (pos2 : rest)) (num+1) dir
    | otherwise = num : rateDirection (getNeighbor pos dir) rest 1 dir

-----------------

-- Returns a list of positions creating a sequence to rate
findSeqInDirection :: Game -> Direction -> [Position] -> [Position]
findSeqInDirection (Game board col) dir colList = undefined

-- Sums checks of both ends of positions sequence
checkEnds :: Game -> Direction -> [Position] -> Int
checkEnds game@(Game board col) dir positions = result
  where
    firstEnd = checkEnd game (opposite dir) (head positions)
    secondEnd = checkEnd game dir (last positions)
    result = firstEnd + secondEnd

-- Check one end of sequence in direction from position
-- Returns 0 when end is occupied (by another color)
-- Returns 1 when end is free
checkEnd :: Game -> Direction -> Position -> Int
checkEnd (Game board col) dir pos
  | checkPos (getNeighbor pos dir) board  = 1
  | otherwise                             = 0


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
  2 -> 10
  2.5 -> 15
  3 -> 40
  3.5 -> 50
  4 -> 120
  4.5 -> 200
  5 -> 1000
  _ -> 0

getCurrColPosList :: Game -> [Position]
getCurrColPosList (Game board col) =
  [pos | pos <- getKeys board, compareColors (getPosColor pos board) col]

------------------ game tree ------------------

infTree = Tree.Node 1 [infTree] -- drzewo nieskonczone

------------------ make move ------------------

makeMove = undefined

------------------- victory -------------------

victory :: Game -> Bool
victory game = any (numToWinInAllDirs posList) posList
  where
    posList = getCurrColPosList game

numToWinInAllDirs :: [Position] -> Position -> Bool
numToWinInAllDirs posList pos = any (numToWinInADir pos posList 1) Position.directions

-- Checks if there is a five-in-a-row sequence of positions in a specified dir
numToWinInADir :: Position -> [Position] -> Int -> Direction -> Bool
numToWinInADir pos posList num dir
  | pos `elem` posList && num == numToWin = True
  | pos `elem` posList = True && numToWinInADir (getNeighbor pos dir) posList  (num + 1) dir
  | otherwise = False

------------------- main tester ----------------------


defMain :: IO ()
defMain = do
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
  --let empty = emptyBoard
  --gameLoop empty
  putStrLn ""
  --askPlayer (Game scopeBoard col)


-- levels (lista do poziomu)
-- Data.Tree.Pretty (rysowanie drzewa)
