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

rateDirections = [East, SouthEast, South, SouthWest] -- directions going down the board - map is sorted in the descending order
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
    neighbors = checkPointNeighbors pos board
    newPosList = updatedPosList ++ [p | p <- neighbors, p `notElem` updatedPosList, p /= pos]

-- Generates next possible moves
nextGames :: Game -> Position -> [Position] -> [Game]
nextGames game@(Game board col) pos posList =
  [Game (addToBoard p col board) col | p <- nextMoves game pos posList]

---------------- rate function ----------------

-- Rates board after each move
-- Sums scores of sequences in each direction
rateGame :: Game -> Int
rateGame game@(Game board col) =
  rateAllDirs game colPosList - rateAllDirs opGame opColPosList
  where
    colPosList = getCurrColPosList game -- tu wszystkie pozycje bieżącego koloru
    opGame = Game board (changeColor col)
    opColPosList = getCurrColPosList opGame -- pozycje przeciwnego koloru

-- Returns the sum of all directions rates
rateAllDirs :: Game -> [Position] -> Int
rateAllDirs game colPosList =
  sum (parMap r0 (rateDir game colPosList) rateDirections)

rateDir :: Game -> [Position] -> Direction -> Int
rateDir game colPosList dir = valuesEvaluated
  where
    allSequences = findSeqsInDir (head colPosList) colPosList [] dir -- find sequences in the direction
    seqEvaluated = evaluateAllSeq game dir allSequences -- count how much worth is every sequence
    valuesEvaluated = evaluateValues seqEvaluated -- rate every sequence length and add

-- Returns a list of sequences (positions create a sequence to rate)
-- Gets starting position, all positions of color
-- and direction in which to seek sequences
findSeqsInDir :: Position -> [Position] -> [Position] -> Direction -> [[Position]]
findSeqsInDir _ [] tmp _ = [tmp]
findSeqsInDir pos colorList tmpList dir
  | pos `elem` colorList = findSeqsInDir (getNeighbor pos dir) (List.delete pos colorList) (tmpList ++ [pos]) dir
  | otherwise = tmpList : findSeqsInDir (head colorList) colorList [] dir

-- Applies evaluation function for sequences
evaluateAllSeq :: Game -> Direction -> [[Position]] -> [Float]
evaluateAllSeq game dir = map (evaluateSequence game dir)

-- Checks if the sequence is both side opened or one side or not at all
evaluateSequence :: Game -> Direction -> [Position] -> Float
evaluateSequence game dir posList
  | check == 2 = fromIntegral (length posList) + 0.5
  | check == 1 = fromIntegral (length posList)
  | otherwise = 0
  where
    check = checkEnds game dir posList

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

-- Gets list of rates for each sequence and returns sum of evaluated rates
evaluateValues :: [Float] -> Int
evaluateValues [] = 0
evaluateValues listOfValues = sum (map value listOfValues)

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

-- Returns list of positions occupied by current color
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

testSeq = do
  let empty = Board Map.empty
  let board = addToBoard (Position 2 2) White (addToBoard (Position 4 4) White (addToBoard (Position 4 7) White (addToBoard (Position 6 7) White empty)))
  let board2 = addToBoard (Position 2 5) White (addToBoard (Position 3 3) White (addToBoard (Position 6 7) White (addToBoard (Position 5 7) White board)))
  let board3 = addToBoard (Position 6 2) White (addToBoard (Position 2 6) White (addToBoard (Position 7 7) Black (addToBoard (Position 3 7) Black board2)))
  let game = Game board3 White
  print game
  let list = getCurrColPosList game
  --let seqs = findSeqsInDir (head list) list [] North
  --show seqs
  --let rated = rateDir game list South
  let rated = rateAllDirs game list
  print rated

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
