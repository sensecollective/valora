module Generators.WaterColor
  ( WaterColorCfg(..)
  , waterColor
  ) where

import Control.Monad (liftM2)
import Control.Monad.Random
import qualified Data.Vector as V

import Color.Shaders (Shader(..), translucer)
import Coords.Math (leastDistance)
import Generators.Color (opacityRamp)
import Patterns.Sparkles (sparkles)
import Poly (Poly(..), Extent(..), extent)
import Transformers.Warp
import Transformers.Wiggle

data WaterColorCfg = WaterColorCfg
  { spread :: Double
  , layers :: Int
  , depth :: Int
  , strokes :: Int
  }

waterColor
  :: RandomGen g
  => WaterColorCfg -> Shader -> Poly -> Rand g (V.Vector Poly)
waterColor WaterColorCfg {spread, layers, depth, strokes} shader poly = do
  sparkles' <- sparkles strokes
  V.sequence $ polies $ (1 -) . (leastDistance sparkles')
  where
    polies spatialAdapter = V.generate layers ((polyLayer spatialAdapter) . (const poly))
    polyLayer spatialAdapter poly =
      warp WarpCfg {depth, wiggleCfg = wiggleCfgForFactor spatialAdapter spread} poly
    wiggleCfgForFactor spatialAdapter _ =
      WiggleCfg
      { adaptToNeighbors = True
      , strength = spread * size
      , coverage = Odd
      , expansion = Outward
      , shareSample = False
      , spatialAdapter = Just spatialAdapter
      }
    size = maximum [height, width]
    Extent {height, width, ..} = extent poly
