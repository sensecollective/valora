module Coords.Math
  ( distance
  , midpoint
  ) where

import Coords (Point(..))

distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ x ^ 2 + y ^ 2
  where
    Point {x, y} = abs $ p1 - p2

midpoint :: Point -> Point -> Point
midpoint (Point {x = x1, y = y1}) (Point {x = x2, y = y2}) =
  Point {x = x1 + (x2 - x1) / 2, y = y1 + (y2 - y1) / 2}
