-----------------------------------------------

module Game where

import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Tree
import qualified Data.List as List
import Control.Parallel.Strategies

import Color
import Position
import Board

-----------------------------------------------

rateDirections = [East, SouthEast, South, SouthWest] -- directions going down the board - map is sorted in the descending order
numToWin = 5 -- you can change number of discs in-a-row needed to win (2-5)

-----------------------------------------------

data Game = Game Board Color

instance Show Game where
  show (Game board col) = showBoard board ++ playerLabel col board

instance Eq Game where
  Game b1 c1 == Game b2 c2 = (b1 == b2) && (c1 == c2)

---------------- player label -----------------

playerLabel :: Color -> Board -> String
playerLabel col board = "\nPlayer: " ++ show col ++ "\n"

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
rateDir _ [] _ = 0
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
evaluateSequence _ _ [] = 0
evaluateSequence game dir posList
  | check == 2 = fromIntegral (length posList) + 0.5
  | check == 1 = fromIntegral (length posList)
  | otherwise = 0
  where
    check = checkEnds game dir posList

-- Sums checks of both ends of positions sequence
checkEnds :: Game -> Direction -> [Position] -> Int
checkEnds _ _ [] = 0
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
  3.5 -> 80
  4 -> 140
  4.5 -> 500
  5 -> 5000
  _ -> 0

-- Returns list of positions occupied by current color
getCurrColPosList :: Game -> [Position]
getCurrColPosList (Game board col) =
  [pos | pos <- getKeys board, compareColors (getPosColor pos board) col]

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
nextMoves (Game board col) pos posList  = newPosList
  where
    updatedPosList = List.delete pos posList
    neighbors = checkPointNeighbors pos board
    newPosList = updatedPosList ++ [p | p <- neighbors, p `notElem` updatedPosList, p /= pos]

-- Generates next possible games
nextGames :: Game -> Position -> [Position] -> [Game]
nextGames game@(Game board col) pos posList =
  [Game (addToBoard p col board) col | p <- nextMoves game pos posList]

------------------ game tree ------------------

infTree = Node 1 [infTree] -- drzewo nieskonczone

gameToTree :: Game -> [Position] -> Tree Game
gameToTree (Game board col) neighbors = Node (Game board col) (children neighbors)
    where
        newNeighbors pos = addNotElem (List.delete pos neighbors) (checkPointNeighbors pos board)
        children = foldr (\ pos -> (++) [gameToTree (Game (addToBoard pos col board) (changeColor col)) (newNeighbors pos)]) []

addNotElem :: [Position] -> [Position] -> [Position]
addNotElem list1 list2 = [x | x <- list1, x `notElem` list2] ++ list2

-------------------- maxmin -------------------

-- Gets starting level, depth of tree, color and tree to evaluate
maxmin :: Int -> Int -> Color -> Tree Game -> Int
maxmin k maxK color (Node game list)
  | null list = rateGame game
  | k > maxK && k `mod` 2 == 0 = indexMax
  | k > maxK  && k `mod` 2 /= 0 = indexMin
  | k `mod` 2 == 0 = maximum (map (maxmin (k+1) maxK color) list)
  | otherwise = minimum (map (maxmin (k+1) maxK color) list)
  where
    listM = exploreForest list
    indexMax = unJust (List.elemIndex (maximum listM) listM)
    indexMin = unJust (List.elemIndex (minimum listM) listM)

-- Rates each game in the list
exploreForest :: [Tree Game] -> [Int]
exploreForest [] = []
exploreForest (Node game _ : rest) = rateGame game : exploreForest rest

getMaxMinBoard :: Game -> Position -> [Position] -> (Board, [Position])
getMaxMinBoard game@(Game board col) pos addedPosNeighbors = (boardMaxMin, updatedList)
  where
    updatedList = nextMoves game pos addedPosNeighbors
    Node g list = gameToTree game updatedList
    Node (Game boardMaxMin _) _ = list !! maxmin 1 0 col (Node g list)

getMaxMinBoardBot :: Game -> [Position] -> (Board, [Position])
getMaxMinBoardBot game@(Game board col) addedPosNeighbors = (boardMaxMin, updatedList)
  where
    updatedList = possibleMoves board
    Node g list = gameToTree game updatedList
    Node (Game boardMaxMin _) _ = list !! maxmin 1 0 col (Node g list)

possibleMoves:: Board -> [Position]
possibleMoves (Board b) = [Position x y | x<-[1..19], y<-[1..19], Map.notMember (Position x y) b, hasNeighbors (Position x y) (Map.keys b)]

hasNeighbors:: Position -> [Position] -> Bool
hasNeighbors (Position x y) listOfPosition =
    any (\(xTranslation, yTranslation) -> (Position (x+xTranslation) (y+yTranslation) `elem` listOfPosition)) [(x,y) | x<-[-1..1], y<-[-1..1], not(x==0 && y==0)]


------------------- victory -------------------

victory :: Game -> Bool
victory game = any (numToWinInAllDirs posList) posList
  where
    posList = getCurrColPosList game

numToWinInAllDirs :: [Position] -> Position -> Bool
numToWinInAllDirs posList pos = any (numToWinInADir pos posList 1) Position.directions

-- Checks if there is a numToWin-in-a-row sequence of positions in a specified dir
numToWinInADir :: Position -> [Position] -> Int -> Direction -> Bool
numToWinInADir pos posList num dir
  | pos `elem` posList && num == numToWin = True
  | pos `elem` posList = True && numToWinInADir (getNeighbor pos dir) posList  (num + 1) dir
  | otherwise = False

------------------------ tester -----------------------

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
  let rated = rateGame game
  print rated
