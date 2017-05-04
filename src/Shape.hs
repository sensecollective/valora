module Shape
  ( Polygon(..)
  , Rect(..)
  , Irregular(..)
  , Edge(..)
  , Point(..)
  ) where

import qualified Data.Vector as V

import Position
import Rand

data Edge = Edge
  { start :: Point
  , end :: Point
  } deriving (Eq, Show)

class Polygon p where
  edges :: p -> [Edge]
  vertices :: p -> [Point]
  vertices pol = map (\Edge {start = point} -> point) $ edges pol

data Irregular =
  Irregular [Edge]

instance Polygon Irregular where
  edges (Irregular edges) = edges

-- Rect ((top left), (bottom right))
data Rect = Rect
  { topLeft :: Point
  , bottomRight :: Point
  }

instance Polygon Rect where
  edges Rect {topLeft = topLeft, bottomRight = bottomRight} =
    let topRight = Point {pointX = pointX bottomRight, pointY = pointY topLeft}
        bottomLeft = Point {pointX = pointX topLeft, pointY = pointY bottomRight}
    in [ Edge {start = topLeft, end = topRight}
       , Edge {start = topRight, end = bottomRight}
       , Edge {start = bottomRight, end = bottomLeft}
       , Edge {start = bottomLeft, end = topLeft}
       ]
