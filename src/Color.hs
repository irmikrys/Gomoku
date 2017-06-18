-------------------------------------------

module Color where

import Data.Maybe

-------------------------------------------

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
  |Data.Maybe.isJust c1 = unJust c1 == c2
  |otherwise = False

unJust :: Maybe t -> t
unJust (Just x) = x
