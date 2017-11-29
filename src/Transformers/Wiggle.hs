module Transformers.Wiggle
  ( WiggleCfg(..)
  , WiggleCoverage(..)
  , wiggle
  ) where

import Control.Monad.Random
import qualified Data.Vector as V

import Coords (Point(..))
import Coords.Math (distance)
import Poly (Poly(..))

data WiggleCfg = WiggleCfg
  { adaptToNeighbors :: Bool
  , strength :: Double
  , coverage :: WiggleCoverage
  } deriving (Show, Eq)

data WiggleCoverage
  = All
  | Odd
  deriving (Show, Eq)

wiggle
  :: RandomGen g
  => WiggleCfg -> Poly -> Rand g Poly
wiggle cfg Poly {vertices} = do
  s1 <- getRandomR (-1, 1)
  s2 <- getRandomR (-1, 1)
  return Poly {vertices = wiggleVertices cfg (s1, s2) vertices}

wiggleVertices :: WiggleCfg
               -> (Double, Double)
               -> V.Vector Point
               -> V.Vector Point
wiggleVertices WiggleCfg {adaptToNeighbors, strength, coverage} (s1, s2) vertices =
  V.update vertices updates
  where
    updates = V.mapMaybe (id) $ V.generate (V.length vertices) wiggler'
    neighbor i = vertices V.! (i `mod` (V.length vertices))
    wiggler' i =
      case (coverage, i `mod` 2 == 0) of
        (All, _) -> Just (i, wiggler i $ vertices V.! i)
        (Odd, False) -> Just (i, wiggler i $ vertices V.! i)
        _ -> Nothing
    wiggler i Point {x, y} = Point {x = x', y = y'}
      where
        x' = x + strength * s1 * (adaptiveFactor i)
        y' = y + strength * s2 * (adaptiveFactor i)
    adaptiveFactor i =
      if adaptToNeighbors
        then distance (neighbor (i - 1)) $ neighbor i + 1
        else 1
