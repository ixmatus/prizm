-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- 'Data.Prizm.Color' provides functions for blending colors,
-- convenience blending ('tint' and 'shade'), and adjusting the 'hue',
-- 'lightness', or 'chroma' of a given color.
--
-- These functions require your color to be represented in @CIE
-- L*Ch@. The reason for this is because the @L*CH@ color space
-- represents colors closest to how the human eye sees them. Blending
-- and transforming a color in @L*Ch@ maintains the brightness of the
-- colors more accurately. Here is a link to a blog post by someone
-- detailing the differences and documenting them well:
-- <http://www.stuartdenman.com/improved-color-blending/ Improved Color Blending>.
--
-- NOTE: the transformations between color spaces are intensive, if
-- you have a HEX or RGB color it first needs to be transformed to the
-- 'CIEXYZ' color space, then the 'CIELAB' space, and finally to
-- 'CIELCH'.
--
-- I may at some point try to generalize the function's types but I
-- wanted to first support 'CIELCH' as the transformation format.
----------------------------------------------------------------------------
module Data.Prizm.Color
(
-- * Lightness, hue, and chroma
  lightness
, chroma
, hue
-- * Types
, module Data.Prizm.Types
) where

import           Data.Prizm.Types

------------------------------------------------------------------------------
-- Lightness, hue, and chroma
------------------------------------------------------------------------------
-- | Adjust the lightness / darkness of a color.
lightness :: CIELCH -> Percent -> CIELCH
lightness (CIELCH (CIELCHp l c h)) w =
  CIELCH $ CIELCHp (clamp (l + (100*(pct (pctClamp w)))) 100.0) c h

-- | Adjust the hue of a color.
hue :: CIELCH -> Percent -> CIELCH
hue (CIELCH (CIELCHp l c h)) w =
  CIELCH $ CIELCHp l c (clamp (h + (360*(pct (pctClamp w)))) 360.0)

-- | Adjust the saturation/chroma of a color.
--
-- A maximum chroma value of 120 is assumed here, anything more is
-- generally considered out of gamut.
chroma :: CIELCH -> Percent -> CIELCH
chroma (CIELCH (CIELCHp l c h)) w =
  CIELCH $ CIELCHp l (clamp (c + (120*(pct (pctClamp w)))) 120.0) h

------------------------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------------------------
clamp :: Double -> Double -> Double
clamp i clmp = max (min i clmp) 0.0
