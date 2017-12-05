module Poly.Clipping
  ( intersect
  , inside
  ) where

import qualified Data.Vector as V

import Poly
import Raster.Poly.Scan
import Coords (Point(..))

intersect :: Poly -> Poly -> Bool
intersect a b = inPoly bigger smaller || inPoly smaller bigger
  where
    bigger = maximum [a, b]
    smaller = minimum [a, b]
    inPoly a b = V.any (inside b) $ vertices a

inside :: Poly -> Point -> Bool
inside poly point = inExtent polyExtent point && inScan polyScangon point
  where
    polyExtent = extent poly
    polyScangon = scangon poly
