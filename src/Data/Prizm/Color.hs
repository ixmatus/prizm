-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- @Data.Prizm.Color@ provides functions for blending colors,
-- convenience blending ('tint' and 'shade'), and adjusting the 'hue',
-- 'lightness', or 'chroma' of a given color.
--
-- These functions require your color to be represented in CIE
-- `L\*Ch`. The reason for this is because the `L\*CH` color space
-- represents colors closest to how the human eye sees them. Blending
-- and transforming a color in `L\*Ch` maintains the brightness of the
-- colors more accurately.
--
-- <http://www.stuartdenman.com/improved-color-blending/ Improved Color Blending>
--
-- Please keep in mind, the transformations between color spaces are
-- intensive, if you have a HEX or RGB color it first needs to be
-- transformed to the 'CIEXYZ' color space.
----------------------------------------------------------------------------
module Data.Prizm.Color
(
-- * Blending
  interpolate
, (<|>)
, shade
, tint
-- * Lightness, hue, and chroma
, lightness
, chroma
, hue
-- * Types
, module Data.Prizm.Types
) where

import           Control.Applicative hiding ((<|>))
import           Data.Prizm.Types

------------------------------------------------------------------------------
-- Blending
------------------------------------------------------------------------------

-- | Interpolate two colors with a weight.
--
-- Weight is applied left to right, so if a weight of 25% is supplied,
-- then the color on the left will be multiplied by 25% and the second
-- color will be multiplied by 75%.
--
-- CIE L*Ch is used because the interpolation between the colors is
-- more accurate than L*ab, XYZ, and sRGB color spaces.
interpolate :: Percent -> (CIELCH Double, CIELCH Double) -> CIELCH Double
interpolate w (a,b) =
    let w' = (pct (pctClamp w))
        (CIELCH l c h) = (-) <$> b <*> a
        a' = (*w') <$> (CIELCH l c (shortestPath h))
    in (+) <$> a' <*> a

-- | Blend two colors using an interpolation value of 50%.
(<|>) :: CIELCH Double -> CIELCH Double -> CIELCH Double
(<|>) l r = interpolate 50 (l,r)

-- | Shade a color by blending it using a weight and the color black.
shade :: CIELCH Double -> Percent -> CIELCH Double
shade c w = interpolate (pctClamp w) (c, CIELCH 0.0 0.0 360.0)

-- | Tint a color by blending it using a weight and the color white.
tint :: CIELCH Double -> Percent -> CIELCH Double
tint c w = interpolate (pctClamp w) (c, CIELCH 100.0 0.0 360.0)

------------------------------------------------------------------------------
-- Lightness, hue, and chroma
------------------------------------------------------------------------------

-- | Adjust the lightness / darkness of a color.
lightness :: CIELCH Double -> Percent -> CIELCH Double
lightness (CIELCH l c h) w = (CIELCH (clamp (l + (100*(pct (pctClamp w)))) 100.0) c h)

-- | Adjust the hue of a color.
hue :: CIELCH Double -> Percent -> CIELCH Double
hue (CIELCH l c h) w = (CIELCH l c (clamp (h + (360*(pct (pctClamp w)))) 360.0))

-- | Adjust the saturation/chroma of a color.
--
-- A maximum chroma value of 120 is assumed here, anything more is
-- generally considered out of gamut.
chroma :: CIELCH Double -> Percent -> CIELCH Double
chroma (CIELCH l c h) w = (CIELCH l (clamp (c + (120*(pct (pctClamp w)))) 120.0) h)

------------------------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------------------------

shortestPath :: Double -> Double
shortestPath h | h > 180    = h - 360
               | h < (-180) = h + 360
               | otherwise  = h

pct :: Integer -> Double
pct = (/100) . fromIntegral . (max (-100)) . (min 100)

pctClamp :: Integer -> Integer
pctClamp i = max (min i 100) 0

clamp :: Double -> Double -> Double
clamp i clmp = max (min i clmp) 0.0
