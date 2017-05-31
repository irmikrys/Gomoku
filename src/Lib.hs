module Lib
    (
    someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc put"
--(.) :: (b->c) -> (a->b) -> (a->c)
-- f . g = \x -> f (g x)
--(f . g) x = f (g x)

--curry/uncurry - bierze zamiast krotki argumenty/ zamiast zrgumentow krotke
--implementacja curry/uncarry
-- c f x y = f (x,y)
