module Generators.Ramp
  ( RampCfg(..)
  , ramp
  ) where

import Control.Monad.Random
import qualified Data.Vector as V

data RampCfg = RampCfg
  { jitter :: Double -- factor to multiply a random sample against (0 for no jitter).
  , steps :: Int
  , factor :: Double -- factor to multiply result against.
  , brackets :: Int -- discrete buckets where factor remains constant until next bracket.
  , invert :: Bool -- start with top of ramp
  }

ramp
  :: RandomGen g
  => RampCfg -> (a -> Double -> b) -> a -> Rand g (V.Vector b)
ramp cfg f a = do
  samples <- getRandomRs (-1, 1)
  return $ V.map (f a) $ V.fromList $ rampFactors cfg samples

rampFactors :: RampCfg -> [Double] -> [Double]
rampFactors RampCfg {jitter, steps, factor, brackets, invert} samples =
  zipWith (factor') [0 .. steps] samples
  where
    factor' i sample = invertF $ factor * (proportion $ bracketF i) + (sample * jitter)
    invertF =
      if invert
        then (1 -)
        else id
    bracketF i =
      if brackets <= 1
        then i
        else i `div` (steps `div` brackets)
    proportion i = fromIntegral i / fromIntegral steps
