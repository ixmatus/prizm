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
-- 'Convertible' instances for converting to and from colors in one of
-- the CIE color space representations provided by this library:
-- * 'CIEXYZ'
-- * 'CIELAB'
-- * 'CIELCH'
----------------------------------------------------------------------------
module Data.Prizm.Color.CIE
( module Data.Prizm.Color.CIE.Types
) where

import           Control.Applicative
import           Data.Convertible.Base
import           Data.Convertible.Utils
import           Data.Prizm.Color.CIE.Types
import           Data.Prizm.Color.CIE.Types    as CIE
import qualified Data.Prizm.Color.Constants    as Constants
import           Data.Prizm.Color.Matrices.XYZ
import qualified Data.Prizm.Color.RGB          as RGB
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------
-- | Reference white, 2° observer, d65 illuminant.
--
-- These values came from Bruce Lindbloom's website: <https://web.archive.org/web/20161110173539/http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html Chromatic Adaptation>
--
-- TODO: this should probably be a triple.
-- TODO: move to another module and make the reference white
-- parameterizable by type so different references can be used!
--
-- @[x,y,z]@
--
-- For future reference (also found in the above linked website), here
-- is a list of reference white illuminant values:
--
-- * @A    1.09850 1.00000 0.35585@
-- * @B    0.99072 1.00000 0.85223@
-- * @C    0.98074 1.00000 1.18232@
-- * @D50  0.96422 1.00000 0.82521@
-- * @D55  0.95682 1.00000 0.92149@
-- * @D65  0.95047 1.00000 1.08883@
-- * @D75  0.94972 1.00000 1.22638@
-- * @E    1.00000 1.00000 1.00000@
-- * @F2   0.99186 1.00000 0.67393@
-- * @F7   0.95041 1.00000 1.08747@
-- * @F11  1.00962 1.00000 0.64350@
refWhite :: [Double]
refWhite = [95.047, 100.000, 108.883]

-- | Transform a 'CIE.XYZ' point.
--
-- TODO: should provide *much* better documentation on what this is
-- actually doing in the algorithms.
transformXYZ :: Double -> Double
transformXYZ v | cv > Constants.ζ  = cv
               | otherwise         = (v - 16 / 116) / Constants.ξ
  where cv = v**3

-- | Calculate the hue for a conversion to 'CIE.LCH' from the 'atan2'
-- of the *a* and *b* color opponents of a 'CIE.LAB' color.
--
-- TODO: should provide *much* better documentation on what this is
-- actually doing in the algorithms.
calcLCHHue :: Double -> Double
calcLCHHue v | v > 0        = (v / pi) * 180
               | otherwise  = 360 - ((abs v) / pi) * 180

-- | Transform a 'CIE.LAB' point.
--
-- TODO: should provide *much* better documentation on what this is
-- actually doing in the algorithms.
transformLAB :: Double -> Double
transformLAB v | v > Constants.ζ = v ** (1/3)
               | otherwise       = (Constants.ξ * v) + (16 / 116)

-- | Transform an 'CIE.XYZ' 'Double' to be computed against the
-- xyzToRGB matrix.
transformRGB :: Double -> Integer
transformRGB v | v > 0.0031308 = min (round (255 * (1.055 * (v ** (1 / 2.4)) - 0.055))) 255
               | otherwise     = min (round (255 * (12.92 * v))) 255

-- | Convert an XYZ color to an RGB color.
--
-- 'XYZtoRGB' is the pre-calculated illuminant matrix, it is
-- preferable to use 'toRG' as it uses the most "common" one.
toRGBMatrix :: XYZtoRGB -> CIE.XYZ -> RGB
toRGBMatrix (XYZtoRGB m) (CIE.XYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        -- NB: be sure to clamp before converting to a Word8,
        -- otherwise we can overflow!
        [r,g,b] = (fromIntegral . RGB.clamp . transformRGB) <$> ((zipTransform t) <$> m)
    in RGB r g b

------------------------------------------------------------------------------
-- Convertible
------------------------------------------------------------------------------
instance Convertible CIE.LAB CIE.LCH where
  -- | Convert a 'CIE.LAB' to a 'CIE.LCH'
  safeConvert (CIE.LAB l a b) =
    let h = calcLCHHue (atan2 b a)
        c = sqrt ((a^(2 :: Int)) + (b^(2 :: Int)))
    in Right $ CIE.LCH l c h

instance Convertible CIE.LAB CIE.XYZ where
  -- | Convert a 'CIE.LAB' to a 'CIE.XYZ'
  safeConvert (CIE.LAB l a b) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        [nx,ny,nz] = getZipList $ ((*) <$> ZipList (transformXYZ <$> [x,y,z])) <*> ZipList refWhite
    in Right $ CIE.XYZ nx ny nz

instance Convertible CIE.LAB RGB where
  -- | Convert a 'CIE.LAB' to a S'RGB'
  safeConvert = convertVia (undefined :: CIE.XYZ)

instance Convertible CIE.LAB Hex where
  -- | Convert a 'CIE.LAB' to an S'RGB' hexadecimal color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible RGB CIE.LAB where
  -- | Convert a S'RGB' to a 'CIE.LAB'
  safeConvert = convertVia (undefined :: CIE.XYZ)

instance Convertible RGB CIE.LCH where
  -- | Convert a S'RGB' to a 'CIE.LCH'
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible Hex CIE.LAB where
  -- | Convert an S'RGB' hexadecimal color to a 'CIE.LAB'
  safeConvert = convertVia (undefined :: RGB)

instance Convertible Hex CIE.LCH where
  -- | Convert an S'RGB' hexadecimal color to a 'CIE.LCH'
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIE.LCH CIE.LAB where
  -- | Convert a 'CIE.LCH' to a 'CIE.LAB'
  safeConvert (CIE.LCH l c h) =
    let v = h * pi / 180
    in Right $ CIE.LAB l ((cos v)*c) ((sin v)*c)

instance Convertible CIE.LCH RGB where
  -- | Convert a 'CIE.LCH' to a S'RGB'
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible CIE.LCH Hex where
  -- | Convert a 'CIE.LCH' to a RGB hexadecimal representation
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIE.LCH CIE.XYZ where
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible CIE.XYZ RGB where
  -- | Convert a 'CIE.XYZ' to an S'RGB'
  --
  -- This function uses the default d65 illuminant matrix.
  safeConvert = Right . toRGBMatrix d65SRGB

instance Convertible CIE.XYZ Hex where
  -- | Convert a 'CIE.XYZ' to an S'RGB' hexadecimal color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIE.XYZ CIE.LCH where
  -- | Convert a 'CIE.XYZ' to a 'CIE.LCH' via 'CIE.LAB'
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible CIE.XYZ CIE.LAB where
  -- | Convert an 'CIE.XYZ' to a 'CIE.LAB'
  --
  -- This function uses the default reference white (2deg observer,
  -- d65 illuminant).
  safeConvert (CIE.XYZ x y z) =
    let v = getZipList $ ZipList ((/) <$> [x,y,z]) <*> ZipList refWhite
        [tx,ty,tz] = (transformLAB) <$> v
        l = (116 * ty) - 16
        a = 500 * (tx - ty)
        b = 200 * (ty - tz)
    in Right $ CIE.LAB l a b

instance Convertible Hex CIE.XYZ where
  -- | Convert a hexadecimal S'RGB' color to a 'CIE.XYZ'
  safeConvert = convertVia (undefined :: RGB)
