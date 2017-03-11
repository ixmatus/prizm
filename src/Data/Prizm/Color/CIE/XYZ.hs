{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE.XYZ
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- Transformation functions and convenience functions, some imported
-- from the other modules to ease conversion between representations.
----------------------------------------------------------------------------
module Data.Prizm.Color.CIE.XYZ
(
-- * Transform to
-- ** RGB or Hex
  toRGB
, toRGBMatrix

-- ** CIE LAB or LCH
, toLAB
) where

import           Control.Applicative
import           Data.Convertible.Base
import           Data.Convertible.Utils

import           Data.Prizm.Color.CIE          (refWhite, v1, v2)
import           Data.Prizm.Color.Matrices.XYZ
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types

import qualified Data.Prizm.Color.SRGB         as S

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Transform an XYZ integer to be computed against the xyzToRGB
-- matrix.
transformRGB :: Double -> Integer
transformRGB v | v > 0.0031308 = min (round ((1.055 * (v ** (1 / 2.4)) - 0.055) * 255)) 255
               | otherwise     = min (round ((12.92 * v) * 255)) 255

-- | Transform an LAB integer.
transformLAB :: Double -> Double
transformLAB v | v > v1    = v ** (1/3)
               | otherwise = (v2 * v) + (16 / 116)

------------------------------------------------------------------------------
-- Convertible instances
------------------------------------------------------------------------------

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
  safeConvert (CIEXYZ (CIEXYZp x y z)) =
    let v = getZipList $ ZipList ((/) <$> [x,y,z]) <*> ZipList refWhite
        [tx,ty,tz] = (transformLAB) <$> v
        l = (116 * ty) - 16
        a = 500 * (tx - ty)
        b = 200 * (ty - tz)
    in Right $ CIELAB (CIELABp l a b)

instance Convertible Hex CIEXYZ where
  -- | Convert a hexadecimal S'RGB' color to a 'CIEXYZ'
  safeConvert = convertVia (undefined :: RGB)

-- | Convert an XYZ color to an SRGB color.
--
-- 'XYZtoRGB' is the pre-calculated illuminant matrix, it is
-- preferable to use 'toRG' as it uses the most "common" one.
toRGBMatrix :: XYZtoRGB -> CIEXYZ -> RGB
toRGBMatrix (XYZtoRGB m) (CIEXYZ (CIEXYZp x y z)) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (fromIntegral . transformRGB) <$> ((zipTransform t) <$> m)
    in  RGB (S.clamp <$> RGBp r g b)
