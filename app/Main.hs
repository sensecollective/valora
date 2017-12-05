{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array.Repa.IO.BMP (writeImageToBMP)
import qualified Data.Vector as V
import System.Random

import Color (HSVA(..), hsva)
import Color (standardBlender)
import Color.Shaders (Shader(..), staticFill)
import Control.Monad.Random (runRand)
import Coords (Point(..))
import Coords.Math (distance)
import Generators.Pack
import Generators.WaterColor
import Patterns.Sparkles (sparkles)
import Poly (centroid)
import Poly.Shapes (ngon)
import Poly.Translations
import Raster (render, rasterWith, emptyRaster)
import Raster.Mask (rasterMasks)
import Raster.Poly.Scan (scanRaster)

radialBlue :: Point -> Shader
radialBlue point _ =
  hsva
    HSVA
    {hue = 360 * (distance point Point {x = 0.5, y = 0.5}), value = 1, saturation = 1, alpha = 1}

main :: IO ()
main = writeImageToBMP "new.bmp" $ render preraster
  where
    preraster = rasterMasks (standardBlender) rasters
    rasters = V.map (uncurry scanRaster) $ V.map (matchShader) triangles
    matchShader triangle = (radialBlue $ centroid triangle, triangle)
    cfg = WaterColorCfg {spread = 3, layers = 20, depth = 4, strokes = 2}
    (triangles, _) =
      runRand
        (pack
           PackCfg
           {src = ngon 0.04 8 $ fromInteger 0, attempts = 10000, shrinkFactor = 1, bounds = Nothing}) $
      mkStdGen 11
