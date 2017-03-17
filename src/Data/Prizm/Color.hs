{-# LANGUAGE ConstrainedClassMethods #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- @Data.Prizm.Color@ exports all of the specific color space modules
-- and types for blending colors, convenience blending ('tint' and
-- 'shade'), and adjusting the 'hue', 'lightness', or 'chroma' of a
-- given color. You should import this module.
--
-- Note that blending colors in a polar-coordinate color space, such
-- as 'CIELCH', preserves the saturation and brightness of the colors
-- being interpolated better than blending in RGB. It also turns out
-- to be an effective even when converting from 'RGB' to 'CIELCH',
-- blending, then converting back again!
--
-- Here is an excellent blog post by someone discussing the differences:
-- <https://web.archive.org/web/20150214062932/http://www.stuartdenman.com/improved-color-blending Improved Color Blending>.
----------------------------------------------------------------------------
module Data.Prizm.Color
(
-- * Color Transformations
  BlendableColor(..)
, AdjustableColor(..)
-- * Preset Colors
, PresetColor(..)
-- * Individual Color Spaces
-- ** CIE Color Space
, module Data.Prizm.Color.CIE
-- ** RGB Color Space
, module Data.Prizm.Color.RGB
-- * Package Types (re-exports the individual color space type modules too)
, module Data.Prizm.Types
) where

import           Data.MonoTraversable
import           Data.Prizm.Color.CIE ()
import qualified Data.Prizm.Color.CIE as CIE
import           Data.Prizm.Color.RGB ()
import           Data.Prizm.Types

-- | Preset white and black for a color space.
class PresetColor c where
  white :: c
  black :: c

-- | A blendable color.
class BlendableColor c where

  -- | Linear interpolation of a color with another color, applying a
  -- weight.
  interpolate :: Percent -> (c,c) -> c

  -- | Blend two 'BlendableColor' colors using an interpolation weight
  -- of 50%.
  (<~>) :: c -> c -> c
  (<~>) l r = interpolate 50 (l,r)

  -- | Shade a color by blending it using a weight and the
  -- @PresetColor@ black.
  shade :: PresetColor c => c -> Percent -> c
  shade c w = interpolate (pctClamp w) (c, black)

  -- | Tint a color by blending it using a weight and the
  -- @PresetColor@ white.
  tint :: PresetColor c => c -> Percent -> c
  tint c w = interpolate (pctClamp w) (c, white)

-- | An adjustable color.
class AdjustableColor c where
  -- | Adjust the lightness of a color
  lightness :: c -> Percent -> c

  -- | Adjust the chroma of a color
  --
  -- NB: not all color spaces will support this easily but it should
  -- be possible to convert into a color space that does then convert
  -- back
  chroma    :: c -> Percent -> c

  -- | Adjust the hue of a color
  hue       :: c -> Percent -> c

instance PresetColor CIE.LCH where
  white = CIE.LCH 0.0 0.0 360.0
  black = CIE.LCH 100.0 0.0 360.0

instance PresetColor RGB where
  white = RGB 255 255 255
  black = RGB 0   0   0

instance BlendableColor CIE.LCH where
  -- | Interpolate two colors in the @CIE L*C*h@ color space with a
  -- weight.
  --
  -- Weight is applied left to right, so if a weight of 25% is supplied,
  -- then the color on the left will be multiplied by 25% and the second
  -- color will be multiplied by 75%.
  interpolate w ((CIE.LCH al ac ah), (CIE.LCH bl bc bh)) =
    let w' = pct w
        (CIE.LCH nl nc nh) = omap (*w') (CIE.LCH (bl - al) (bc - ac) (shortestPath (bh - ah)))
    in CIE.LCH (al + nl) (ac + nc) (ah + nh)

instance AdjustableColor CIE.LCH where
  -- | Adjust the lightness / darkness of a color.
  lightness (CIE.LCH l c h) w =
    CIE.LCH (clamp (l + (100*(pct (pctClamp w)))) 100.0) c h

  -- | Adjust the hue of a color.
  hue (CIE.LCH l c h) w =
    CIE.LCH l c (clamp (h + (360*(pct (pctClamp w)))) 360.0)

  -- | Adjust the saturation/chroma of a color.
  --
  -- A maximum chroma value of 120 is assumed here, anything more is
  -- generally considered out of gamut.
  chroma (CIE.LCH l c h) w =
    CIE.LCH l (clamp (c + (120*(pct (pctClamp w)))) 120.0) h

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------
-- | Give the shortest path to the hue value.
shortestPath :: Double -> Double
shortestPath h | h > 180    = h - 360
               | h < (-180) = h + 360
               | otherwise  = h

-- | Give the decimal value for the given "percent" value.
--
-- The @Percent@ value may range from -100 to 100.
pct :: Percent -> Double
pct i = fromIntegral m / 100
  where
    m = pctClamp i

-- | Clamp a 'Percent' value in the range -100 to 100.
pctClamp :: Percent -> Percent
pctClamp i = max (min i 100) (-100)

-- | Clamp a 'Double' with a bottom of at least 0.0.
clamp :: Double -> Double -> Double
clamp i clmp = max (min i clmp) 0.0
