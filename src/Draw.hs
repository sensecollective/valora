module Draw
  ( draw
  ) where

import qualified Data.HashMap as H
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V

import Img
import Shape

data ScanEdge = ScanEdge
  { low :: Double
  , high :: Double
  , slope :: Double -> Double
  }

data DrawContext = DrawContext
  { activeEdges :: V.Vector ScanEdge
  , remainingEdges :: V.Vector ScanEdge
  , scanLine :: Double
  , canvas :: H.Map Point RGBA
  , color :: RGBA
  }

draw
  :: Polygon p
  => RGBA -> p -> H.Map Point RGBA
draw color pol =
  let (DrawContext {canvas = result}) =
        until
          drawDone
          drawStep
          DrawContext
          { activeEdges = V.filter (inScanRange scanLine) edges
          , remainingEdges = V.filter (not . inScanRange scanLine) edges
          , scanLine = scanLine
          , canvas = H.empty
          , color = color
          }
  in result
  where
    ScanEdge {low = scanLine} = V.head edges
    edges =
      V.modify
        (V.sortBy (\(ScanEdge {low = l1}) (ScanEdge {low = l2}) -> compare l1 l2))
        (scanEdges pol)

drawDone :: DrawContext -> Bool
drawDone (DrawContext {activeEdges = ae}) = V.null ae

drawStep :: DrawContext -> DrawContext
drawStep (DrawContext { activeEdges = activeEdges
                      , remainingEdges = remainingEdges
                      , scanLine = scanLine
                      , canvas = canvas
                      , color = color
                      }) =
  DrawContext
  { activeEdges = activeEdges'
  , remainingEdges = V.filter (not . inScanRange (scanLine')) remainingEdges
  , scanLine = scanLine'
  , canvas = H.union canvas $ H.unions $ map H.unions $ map fillSegment segmentsToFill
  , color = color
  }
  where
    scanLine' = scanLine + 1
    segmentsToFill = overlaps $ intersects scanLine activeEdges
    fillSegment (x1, x2) = map (fillPixel) [x1 .. x2]
    fillPixel x = H.insert (Point {pointX = x, pointY = scanLine}) color H.empty
    activeEdges' =
      V.modify
        (V.sortBy
           (\(ScanEdge {slope = slope1}) (ScanEdge {slope = slope2}) ->
              compare (slope1 scanLine') (slope2 scanLine')))
        (V.filter (inScanRange (scanLine')) (V.concat [remainingEdges, activeEdges]))

inScanRange :: Double -> ScanEdge -> Bool
inScanRange scanLine (ScanEdge {low = low, high = high}) = scanLine >= low && scanLine <= high

intersects :: Double -> V.Vector ScanEdge -> V.Vector Double
intersects scanLine es = V.map (\(ScanEdge {slope = slope}) -> slope scanLine) es

overlaps :: V.Vector Double -> [(Double, Double)]
overlaps xs =
  if length xs < 2
    then []
    else [(V.head xs, V.head (V.tail xs))] ++ overlaps (V.tail xs)

scanEdge :: Edge -> ScanEdge
scanEdge (Edge {start = Point {pointX = x1, pointY = y1}, end = Point {pointX = x2, pointY = y2}}) =
  ScanEdge {low = starty, high = max y1 y2, slope = (\y -> startx + (m * (y - starty)))}
  where
    starty = min y1 y2
    startx =
      if starty == y1
        then x1
        else x2
    deltaX = x2 - x1
    deltaY = y2 - y1
    m =
      if deltaY == 0
        then 0
        else (deltaX / deltaY)

scanEdges
  :: Polygon p
  => p -> V.Vector ScanEdge
scanEdges pol = V.map (scanEdge) $ V.fromList $ edges pol
