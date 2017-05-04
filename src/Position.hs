{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Position
  ( Point(..)
  ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Point = Point
  { pointX :: Double
  , pointY :: Double
  } deriving (Eq, Ord, Generic, Hashable, Show)
