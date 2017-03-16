{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Basic utility functions for the @CIE@ transformations and @CIE@
-- convertible instances between the different color space
-- representations within @CIE@ and @RGB@.
----------------------------------------------------------------------------
module Data.Prizm.Color.CIE
( clamp
, refWhite
, toRGBMatrix
, transformLAB
, calcLCHHue
, transformRGB
, transformXYZ
, v1
, v2
) where

import           Control.Applicative
import           Data.Convertible.Base
import           Data.Convertible.Utils
import           Data.MonoTraversable
import           Data.Prizm.Color.Matrices.XYZ
import qualified Data.Prizm.Color.SRGB         as S
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types

instance PresetColor CIELCH where
  white = CIELCH 0.0 0.0 360.0
  black = CIELCH 100.0 0.0 360.0

instance BlendableColor CIELCH where
  -- | Interpolate two colors in the @CIE L*C*h@ color space with a
  -- weight.
  --
  -- Weight is applied left to right, so if a weight of 25% is supplied,
  -- then the color on the left will be multiplied by 25% and the second
  -- color will be multiplied by 75%.
  interpolate w ((CIELCH al ac ah), (CIELCH bl bc bh)) =
    let w' = pct w
        (CIELCH nl nc nh) = omap (*w') (CIELCH (bl - al) (bc - ac) (shortestPath (bh - ah)))
    in CIELCH (al + nl) (ac + nc) (ah + nh)

instance AdjustableColor CIELCH where
  -- | Adjust the lightness / darkness of a color.
  lightness (CIELCH l c h) w =
    CIELCH (clamp (l + (100*(pct (pctClamp w)))) 100.0) c h

  -- | Adjust the hue of a color.
  hue (CIELCH l c h) w =
    CIELCH l c (clamp (h + (360*(pct (pctClamp w)))) 360.0)

  -- | Adjust the saturation/chroma of a color.
  --
  -- A maximum chroma value of 120 is assumed here, anything more is
  -- generally considered out of gamut.
  chroma (CIELCH l c h) w =
    CIELCH l (clamp (c + (120*(pct (pctClamp w)))) 120.0) h

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------
clamp :: Double -> Double -> Double
clamp i clmp = max (min i clmp) 0.0

-- | Exact rational of the "0.008856" value.
v1 :: Double
v1 = (6/29) ** 3

-- | Exact rational of the "7.787" value.
v2 :: Double
v2 = 1/3 * ((29/6) ** 2)

-- | Reference white, 2deg observer, d65 illuminant.
--
-- @[x,y,z]@
--
-- TODO: this should probably be a triple.
refWhite :: [Double]
refWhite = [95.047, 100.000, 108.883]

-- | Transform a 'CIEXYZ' point.
--
-- TODO: should provide *much* better documentation on what this is
-- actually doing in the algorithms.
transformXYZ :: Double -> Double
transformXYZ v | cv > v1   = cv
               | otherwise = (v - 16 / 116) / v2
  where cv = v**3

-- | Calculate the hue for a conversion to 'CIELCH' from the 'atan2'
-- of the *a* and *b* color opponents of a 'CIELAB' color.
--
-- TODO: should provide *much* better documentation on what this is
-- actually doing in the algorithms.
calcLCHHue :: Double -> Double
calcLCHHue v | v > 0      = (v / pi) * 180
               | otherwise  = 360 - ((abs v) / pi) * 180

-- | Transform a 'CIELAB' point.
--
-- TODO: should provide *much* better documentation on what this is
-- actually doing in the algorithms.
transformLAB :: Double -> Double
transformLAB v | v > v1    = v ** (1/3)
               | otherwise = (v2 * v) + (16 / 116)

-- | Transform an 'CIEXYZ' 'Double' to be computed against the
-- xyzToRGB matrix.
transformRGB :: Double -> Integer
transformRGB v | v > 0.0031308 = min (round (255 * (1.055 * (v ** (1 / 2.4)) - 0.055))) 255
               | otherwise     = min (round (255 * (12.92 * v))) 255

-- | Convert an XYZ color to an SRGB color.
--
-- 'XYZtoRGB' is the pre-calculated illuminant matrix, it is
-- preferable to use 'toRG' as it uses the most "common" one.
toRGBMatrix :: XYZtoRGB -> CIEXYZ -> RGB
toRGBMatrix (XYZtoRGB m) (CIEXYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        -- NB: be sure to clamp before converting to a Word8,
        -- otherwise we can overflow!
        [r,g,b] = (fromIntegral . S.clamp . transformRGB) <$> ((zipTransform t) <$> m)
    in RGB r g b

------------------------------------------------------------------------------
-- Convertible
------------------------------------------------------------------------------

instance Convertible CIELAB CIELCH where
  -- | Convert a 'CIELAB' to a 'CIELCH'
  safeConvert (CIELAB l a b) =
    let h = calcLCHHue (atan2 b a)
        c = sqrt ((a^(2 :: Int)) + (b^(2 :: Int)))
    in Right $ CIELCH l c h

instance Convertible CIELAB CIEXYZ where
  -- | Convert a 'CIELAB' to a 'CIEXYZ'
  safeConvert (CIELAB l a b) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        [nx,ny,nz] = getZipList $ ((*) <$> ZipList (transformXYZ <$> [x,y,z])) <*> ZipList refWhite
    in Right $ CIEXYZ nx ny nz

instance Convertible CIELAB RGB where
  -- | Convert a 'CIELAB' to a S'RGB'
  safeConvert = convertVia (undefined :: CIEXYZ)

instance Convertible CIELAB Hex where
  -- | Convert a 'CIELAB' to an S'RGB' hexadecimal color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible RGB CIELAB where
  -- | Convert a S'RGB' to a 'CIELAB'
  safeConvert = convertVia (undefined :: CIEXYZ)

instance Convertible RGB CIELCH where
  -- | Convert a S'RGB' to a 'CIELCH'
  safeConvert = convertVia (undefined :: CIELAB)

instance Convertible Hex CIELAB where
  -- | Convert an S'RGB' hexadecimal color to a 'CIELAB'
  safeConvert = convertVia (undefined :: RGB)

instance Convertible Hex CIELCH where
  -- | Convert an S'RGB' hexadecimal color to a 'CIELCH'
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIELCH CIELAB where
  -- | Convert a 'CIELCH' to a 'CIELAB'
  safeConvert (CIELCH l c h) =
    let v = h * pi / 180
    in Right $ CIELAB l ((cos v)*c) ((sin v)*c)

instance Convertible CIELCH RGB where
  -- | Convert a 'CIELCH' to a S'RGB'
  safeConvert = convertVia (undefined :: CIELAB)

instance Convertible CIELCH CIEXYZ where
  safeConvert = convertVia (undefined :: CIELAB)

instance Convertible CIEXYZ RGB where
  -- | Convert a 'CIEXYZ' to an S'RGB'
  --
  -- This function uses the default d65 illuminant matrix.
  safeConvert = Right . toRGBMatrix d65SRGB

instance Convertible CIEXYZ Hex where
  -- | Convert a 'CIEXYZ' to an S'RGB' hexadecimal color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIEXYZ CIELCH where
  -- | Convert a 'CIEXYZ' to a 'CIELCH' via 'CIELAB'
  safeConvert = convertVia (undefined :: CIELAB)

instance Convertible CIEXYZ CIELAB where
  -- | Convert an 'CIEXYZ' to a 'CIELAB'
  --
  -- This function uses the default reference white (2deg observer,
  -- d65 illuminant).
  safeConvert (CIEXYZ x y z) =
    let v = getZipList $ ZipList ((/) <$> [x,y,z]) <*> ZipList refWhite
        [tx,ty,tz] = (transformLAB) <$> v
        l = (116 * ty) - 16
        a = 500 * (tx - ty)
        b = 200 * (ty - tz)
    in Right $ CIELAB l a b

instance Convertible Hex CIEXYZ where
  -- | Convert a hexadecimal S'RGB' color to a 'CIEXYZ'
  safeConvert = convertVia (undefined :: RGB)
