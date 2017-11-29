{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import qualified Data.Vector as V
import System.Random

import Color (RGBA(..))
import Color (standardBlender)
import Color.Shaders (Shader(..), staticFill)
import Control.Monad.Random (runRand)
import Coords (Point(..))
import Generators.WaterColor
import Poly.Shapes (square)
import Rand.Normal
import Raster (render, rasterWith, emptyRaster)
import Raster.Mask (rasterMasks)
import Raster.Poly.Scan (scanRaster)

main :: IO ()
main = writeImageToBMP "new.bmp" $ render preraster
  where
    preraster = rasterMasks (standardBlender) rasters
    rasters = V.map (uncurry scanRaster) shadedSqs
    shadedSqs = fst $ runRand waterBrush (Normal $ mkStdGen 11)
    waterBrush =
      waterColor
        WaterColorCfg {spread = 0.03, layers = 1, depth = 3, maxOpacity = 0.2}
        shader
        sq
    sq = square Point {x = 0.2, y = 0.2} 0.4
    shader = staticFill RGBA {red = 1, blue = 1, green = 0, alpha = 1}
