-------------------------------------------

module Position where

-------------------------------------------

data Direction =
  NorthEast | North | NorthWest | West |
  SouthWest | South | SouthEast | East
    deriving Show

opposite :: Direction -> Direction
opposite dir = case dir of
  West -> East
  SouthWest -> NorthEast
  South -> North
  SouthEast -> NorthWest

directions = [NorthEast, North, NorthWest, West, SouthWest, South, SouthEast, East]

-------------------------------------------

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
getPointNeighbors :: Position  -> [Position]
getPointNeighbors pos = [getNeighbor pos dir | dir <- directions]
