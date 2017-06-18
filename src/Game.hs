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

getCurrColPosList :: Game -> [Position]
getCurrColPosList (Game board col) =
  [pos | pos <- getKeys board, compareColors (getPosColor pos board) col]

------------------ game tree ------------------

infTree = Tree.Node 1 [infTree] -- drzewo nieskonczone

------------------ make move ------------------

makeMove = undefined

------------------- victory -------------------

victory :: Game -> Bool
victory game = any (fiveInAllDirs posList) posList
  where
    posList = getCurrColPosList game

fiveInAllDirs :: [Position] -> Position -> Bool
fiveInAllDirs posList pos = any (fiveInADir pos posList 1) Position.directions

-- Checks if there is a five-in-a-row sequence of positions in a specified dir
fiveInADir :: Position -> [Position] -> Int -> Direction -> Bool
fiveInADir pos posList num dir
  | pos `elem` posList && num == 5 = True
  | pos `elem` posList = True && fiveInADir (getNeighbor pos dir) posList  (num + 1) dir
  | otherwise = False

------------------- main tester ----------------------


main :: IO ()
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
  --let empty = emptyBoard
  --gameLoop empty
  putStrLn ""
  --askPlayer (Game scopeBoard col)


-- levels (lista do poziomu)
-- Data.Tree.Pretty (rysowanie drzewa)
