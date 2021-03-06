module Img
  ( Raster
  , Layer
  , fromMap
  , newLayer
  , fillLayer
  , applyLayer
  , applyPixel
  , addPixel
  , rasterLayer
  , RGBA
  ) where

import Data.Array.Repa (Array, DIM2, DIM3, Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.HashMap as H
import Data.Word (Word8)

import Shape

type Raster = Array R.U DIM2 Dot

type Dot = (Word8, Word8, Word8)

type RGBA = (Double, Double, Double, Double)

type Layer = Array R.D DIM2 RGBA

fromMap :: (Int, Int) -> H.Map (Int, Int) RGBA -> Layer
fromMap (w, h) map = R.traverse (newLayer (w, h)) id maybeColor
  where
    maybeColor indx (Z :. y :. x) = H.findWithDefault (0, 0, 0, 0) (x, y) map

newLayer :: (Int, Int) -> Layer
newLayer (w, h) = R.traverse (raw (h, w)) packDims packPixel
  where
    packDims (Z :. h :. w :. c) = (Z :. h :. w)
    packPixel indx (Z :. y :. x) =
      ( indx (Z :. y :. x :. 0)
      , indx (Z :. y :. x :. 1)
      , indx (Z :. y :. x :. 2)
      , indx (Z :. y :. x :. 3))

fillLayer :: RGBA -> Layer -> Layer
fillLayer color layer = R.map (\_ -> applyPixel (0, 0, 0, 0) color) layer

applyLayer :: Layer -> Layer -> Layer
applyLayer bottom top = R.traverse bottom id applyPixelProxy
  where
    applyPixelProxy indx pos = applyPixel (indx pos) (top R.! pos)

rasterLayer :: Layer -> Raster
rasterLayer layer =
  let [img] = R.computeP $ R.map rasterPixel layer
  in img

raw :: (Int, Int) -> Array R.U DIM3 Double
raw (w, h) = R.fromListUnboxed (Z :. w :. h :. 4) (take (w * h * 4) (cycle [0]))

applyPixel (br, bg, bb, ba) (fr, fg, fb, fa) = (blend br fr, blend bg fg, blend bb fb, denom)
  where
    denom = fa + ba * (1 - fa)
    blend b f = f * fa + b * (1 - fa)

addPixel (br, bg, bb, ba) (fr, fg, fb, fa) = (add br fr, add bg fg, add bb fb, a)
  where
    a = fa + ba * (1 - fa)
    add b f = f + b

rasterPixel (r, g, b, _) = (f r, f g, f b)
  where
    f = floor . (* 255)
