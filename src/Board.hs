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

------------ globals ----------------

myBoard = Board Map.empty
--newBoard = addToBoard (Position 3 4) Black myBoard
--newBoard2 = addToBoard (Position 10 13) White newBoard

nums = [1,2..19]
chars = ['A'..'S']
currCol = White
move = 0

-------------------------------------

data Color =
  Black
  | White

instance Show Color where
  show Black = "\9787"
  show White = "\9786"

instance Read Color where
  readsPrec _ input
    | head input == 'W' = [(White, tail input)]
    | head input == 'B' = [(Black, tail input)]
    | otherwise = []

-------------------------------------

data Position = Position Int Int

instance Show Position where
  show (Position x1 x2) = "Pos(" ++ show x1 ++ "," ++ show x2 ++ ")"

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

newtype Board = Board(Map.Map Position Color)

instance Show Board where
  show = showBoard

addToBoard :: Position -> Color -> Board -> Board
addToBoard pos col (Board prevMap) = Board(Map.insert pos col prevMap)

---------- showing board ------------

showBoard :: Board -> String
showBoard board = "\n" ++ upDownLabel ++ getAllStringRows board ++ upDownLabel ++ playerLabel move

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

playerLabel :: Int -> String
playerLabel move
   | move `mod` 2 == 0  = "\nPlayer: " ++ show White ++ "\n"
   | otherwise          = "\nPlayer: " ++ show Black ++ "\n"

----------- player input ------------

askPlayer :: Color -> IO ()
askPlayer col =
  do
    putStrLn ("Player: " ++ show col ++ ", choose position: ")
    x <- getLine
    y <- getLine
    let pos = Position (read x) (read y)
    print pos

getFromPlayer :: Color -> Board -> Board
getFromPlayer col board = undefined

--------- free positions ------------

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

getAllFreePos :: Board -> [Position]
getAllFreePos board = [Position i j | i <- nums, j <- nums, checkPos (Position i j) board]

---------- kolejne ruchy ------------



------------- helpful ---------------

getBare :: Maybe t -> t
getBare (Just x) = x

charToString :: Char -> String
charToString c = [c]

-------------- main -----------------

main = do
  putStrLn ("\n============== " ++ show White ++ " Gomoku " ++ show Black ++ " ===============")
  print myBoard


-- levels (lista do poziomu)
