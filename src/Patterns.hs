module Patterns
  ( tile
  ) where

import qualified Data.HashMap as H

import Shape

tile :: Int -> Int -> [Rect]
tile frame tiles = rects
  where
    rects =
      map
        (\(x, y) ->
           Rect
           { topLeft = Point {pointX = x, pointY = y}
           , bottomRight = Point {pointX = x + tileSize, pointY = y + tileSize}
           }) $
      concat corners
    corners = map (\b -> zip borders (repeat b)) borders
    borders = take tiles $ 0 : zipWith (+) borders (repeat tileSize)
    tileSize = (fromIntegral frame) / (fromIntegral tiles)
