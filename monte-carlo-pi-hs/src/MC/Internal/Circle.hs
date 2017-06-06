module MC.Internal.Circle (Point(..)
                          , isInUnitCircle) where

import System.Random

data Point = Point Double Double deriving (Show, Eq)

instance Random Point where
  random = randomR (Point (negate 1) (negate 1), Point 1 1)
  randomR (Point xs ys, Point xe ye) g =
    let (x, g') = randomR (xs, xe) g
        (y, g'') = randomR (ys, ye) g'
    in (Point x y, g'')

isInUnitCircle :: Point -> Bool
isInUnitCircle (Point x y) = (x ** 2) + (y ** 2) <= 1
