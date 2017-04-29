{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.RGB.Types
-- Copyright   :  (C) 2017 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Color.RGB.Types where

import           Data.Prizm.Types
import           Data.Word

-- | Clamp a 'Word8' with an upper-bound of 255 and a lower-bound of
-- 0.
clamp :: Integral a => a -> a
clamp i = max (min i 255) 0

-- | A color in the 256-cubed @RGB@ color space.
newtype RGB = RGB {unRGB :: ColorCoord Word8 }
  deriving (Eq, Ord, Show)

-- | Produce a 256-cubed 'RGB' color.
--
-- NB: this function clamps each argument to the 0-255 range.
mkRGB :: Int -- ^ Red color channel
      -> Int -- ^ Green color channel
      -> Int -- ^ Blue color channel
      -> RGB
mkRGB r g b = RGB ((fromIntegral . clamp) <$> ColorCoord (r,g,b))
