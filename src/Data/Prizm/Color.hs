-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- 'Data.Prizm.Color' exports instances for blending colors,
-- convenience blending ('tint' and 'shade'), and adjusting the 'hue',
-- 'lightness', or 'chroma' of a given color.
--
-- Currently, there are only instances for colors in the in @CIE L*Ch@
-- color space. The reason for this is because the @CIE L*Ch@ color
-- space represents the hue of a color closest to how the human eye
-- perceives it. Blending and transforming a color in @CIE L*Ch@
-- maintains the brightness of the colors more accurately than the
-- other color spaces.
--
-- Here is a link to a blog post by someone detailing the differences
-- and documenting them well:
-- <https://web.archive.org/web/20150214062932/http://www.stuartdenman.com/improved-color-blending Improved
-- Color Blending>.
----------------------------------------------------------------------------
module Data.Prizm.Color
(
-- * Convertible Instances
-- ** CIE Color Space
  module Data.Prizm.Color.CIE
-- ** SRGB Color Space
, module Data.Prizm.Color.SRGB
-- * Types
, module Data.Prizm.Types
) where

import           Data.Prizm.Color.CIE  hiding (clamp)
import           Data.Prizm.Color.SRGB hiding (clamp)
import           Data.Prizm.Types
