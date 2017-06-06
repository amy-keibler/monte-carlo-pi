module MC.Pi (approximatePi) where

import MC.Internal.Circle

approximatePi :: [Point] -> Double
approximatePi points = 4.0 * fromIntegral pointsInCircle / fromIntegral totalPoints
  where pointsInCircle = length $ filter isInUnitCircle points
        totalPoints = length points
