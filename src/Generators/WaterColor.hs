module Generators.WaterColor
  ( WaterColorCfg(..)
  , waterColor
  ) where

import Control.Monad (liftM2)
import Control.Monad.Random
import qualified Data.Vector as V

import Color (RGBA(..))
import Color.Shaders (Shader(..), translucer)
import Generators.Color (opacityRamp)
import Generators.Ramp
import Poly (Poly(..))
import Poly.Properties (Extent(..), extent)
import Transformers.Warp
import Transformers.Wiggle

data WaterColorCfg = WaterColorCfg
  { spread :: Double
  , layers :: Int
  , depth :: Int
  , maxOpacity :: Double
  }

waterColor
  :: RandomGen g
  => WaterColorCfg -> Shader -> Poly -> Rand g (V.Vector (Shader, Poly))
waterColor WaterColorCfg {spread, layers, depth, maxOpacity} shader poly = do
  polies' <- polies
  shaders' <- shaders
  polies'' <- V.sequence polies'
  return $ V.zip shaders' polies''
  where
    shaders =
      opacityRamp
        RampCfg {jitter = 0, steps = layers, factor = maxOpacity}
        shader
    polies =
      ramp
        RampCfg {jitter = 0, steps = layers, factor = spread}
        (polyLayer)
        poly
    polyLayer poly factor =
      warp WarpCfg {depth, wiggleCfg = wiggleCfgForFactor factor} poly
    wiggleCfgForFactor factor =
      WiggleCfg {adaptToNeighbors = True, strength = factor, coverage = Odd}

fuse
  :: RandomGen g
  => V.Vector (Rand g a) -> V.Vector (Rand g b) -> Rand g (V.Vector (a, b))
fuse v1 v2 = V.sequence $ V.zipWith (liftM2 (,)) v1 v2
