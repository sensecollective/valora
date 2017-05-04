module Warp
  ( Warper(..)
  , warpDupe
  , warpPoly
  , deepWarpPoly
  , sqTrim
  ) where

import Control.Lens (traverse, both, toListOf)
import Data.Traversable (mapAccumR)

import Rand
import Shape

sqTrim :: Double -> Rect -> Rect
sqTrim trim (Rect { topLeft = Point {pointX = x1, pointY = y1}
                  , bottomRight = Point {pointX = x2, pointY = y2}
                  }) =
  Rect
  { topLeft = Point {pointX = x1 + trim, pointY = y1 + trim}
  , bottomRight = Point {pointX = x2 - trim, pointY = y2 - trim}
  }

data Warper d = Warper
  { dist :: d
  , heat :: (Point -> Double)
  }

warpDupe
  :: (Dist d, Polygon p)
  => Int -> Int -> Warper d -> p -> (Warper d, [Irregular])
warpDupe depth layers warper poly = (warper', warpedPolies)
  where
    (warper', warpedPolies) = mapAccumR (deepWarpPoly depth) warper $ take layers $ repeat poly

deepWarpPoly
  :: (Dist d, Polygon p)
  => Int -> Warper d -> p -> (Warper d, Irregular)
deepWarpPoly n warper poly = iterate (uncurry warpPoly) (warper, (Irregular (edges poly))) !! n

warpPoly
  :: (Dist d, Polygon p)
  => Warper d -> p -> (Warper d, Irregular)
warpPoly warper poly = (warper', Irregular edges')
  where
    edges' = toListOf (traverse . both) edgePairs
    (warper', edgePairs) = mapAccumR (warpEdge) warper (edges poly)

warpEdge
  :: Dist d
  => Warper d -> Edge -> (Warper d, (Edge, Edge))
warpEdge warper (Edge {start = start, end = end}) =
  (warper', (Edge {start = start, end = warped}, Edge {start = warped, end = end}))
  where
    (warper', warped) = warpPoint warper $ midpoint start end

warpPoint
  :: Dist d
  => Warper d -> Point -> (Warper d, Point)
warpPoint (Warper {dist = dist, heat = heat}) (Point {pointX = x, pointY = y}) = (warper', point')
  where
    warper' = Warper {dist = dist', heat = heat}
    point' = Point {pointX = x + xshift * strength, pointY = y + yshift * strength}
    strength = heat $ Point {pointX = x, pointY = y}
    ((xshift, yshift), dist') = randPair dist

midpoint :: Point -> Point -> Point
midpoint (Point {pointX = x1, pointY = y1}) (Point {pointX = x2, pointY = y2}) =
  Point {pointX = x1 + (x2 - x1) / 2, pointY = y1 + (y2 - y1) / 2}
