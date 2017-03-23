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

-- | Clamp a 'Word8' with an upper-bound of 255 (the maximum RGB
-- value).
clamp :: Integral a => a -> a
clamp i = max (min i 255) 0

-- | A color in the @sRGB@ color space.
newtype RGB = RGB {unRGB :: ColorCoord Word8 }
  deriving (Eq, Ord, Show)

-- | Produce an 'RGB' color.
mkRGB :: Word8 -- ^ Red color channel
      -> Word8 -- ^ Green color channel
      -> Word8 -- ^ Blue color channel
      -> RGB
mkRGB r g b = RGB (clamp <$> ColorCoord (r,g,b))
