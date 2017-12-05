module Generators.Pack
  ( pack
  , PackCfg(..)
  ) where

import Control.Monad.Random
import qualified Data.Vector as V

import Generators.Ramp
import Patterns.Sparkles
import Poly (Poly(..))
import Poly.Clipping (intersect, inside)
import Poly.Translations (scale, translate)
import VectorUtil

data PackCfg = PackCfg
  { src :: Poly
  , attempts :: Int
  , shrinkFactor :: Double
  , bounds :: Maybe Poly
  }

pack
  :: RandomGen g
  => PackCfg -> Rand g (V.Vector Poly)
pack PackCfg {src, attempts, shrinkFactor, bounds} = do
  srcs' <- srcs
  places' <- places
  let candidates = V.zipWith (translate) places' srcs'
  return $ V.map (snd) $ V.filter (uncurry (valid candidates)) $ enumerate candidates
  where
    places = sparkles attempts
    srcs =
      ramp
        RampCfg {jitter = 0, steps = attempts, factor = shrinkFactor, brackets = 100, invert = True}
        (flip scale)
        src
    valid polies i candidate = validVsOthers && validVsBounds
      where
        validVsOthers = not $ V.any (intersect candidate) $ V.slice 0 i polies
        validVsBounds =
          case bounds of
            Just bounds -> V.all (inside bounds) $ vertices candidate
            Nothing -> True
