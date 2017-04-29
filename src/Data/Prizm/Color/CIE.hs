{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

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
( clamp
, module Data.Prizm.Color.CIE.Types
) where

import           Control.Applicative
import           Data.Convertible.Base
import           Data.Convertible.Utils
import qualified Data.Prizm.Color.CIE.Chroma.Illuminant as Illuminant
import           Data.Prizm.Color.CIE.Matrices.XYZ
import           Data.Prizm.Color.CIE.Types
import           Data.Prizm.Color.CIE.Types             as CIE
import qualified Data.Prizm.Color.Constants             as Constants
import           Data.Prizm.Color.RGB                   (RGB)
import qualified Data.Prizm.Color.RGB                   as RGB
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Clamp a 'Double' with a bottom of at least 0.0.
clamp :: Double -> Double -> Double
clamp i clmp = max (min i clmp) 0.0

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
toRGBMatrix :: XYZtoRGB -> CIE.XYZ -> RGB.RGB
toRGBMatrix (Matrix m) (unXYZ -> ColorCoord(x,y,z)) =
    let t = ((/100) <$> ZipList [x,y,z])
        -- NB: be sure to clamp before converting to a Word8,
        -- otherwise we can overflow!
        [r,g,b] = (fromIntegral . RGB.clamp . transformRGB) <$> ((zipTransform t) <$> m)
    in RGB.mkRGB r g b

-- | Convert a 'XYZ' color to the 'LAB' color space using the given
-- reference white illuminant.
--
-- NB: the convertible instance uses the 'd65' reference white
-- illuminant, use this function if you need to use a different
-- reference white.
xyzToLAB :: CIE.XYZ -> Illuminant.RefWhite -> CIE.LAB
xyzToLAB (unXYZ -> ColorCoord xyz) (Illuminant.Tristimulus refWhite) =
        -- TODO: figure out how I can use <$$$> lens version with some
        -- kind of applicative-like thing to do the below...
    let v = (/) <$$$> xyz <***> refWhite
        (tx,ty,tz) = ((transformLAB) <$$$> v)
        l = (116 * ty) - 16
        a = 500 * (tx - ty)
        b = 200 * (ty - tz)
    in CIE.mkLAB l a b

-- | Convert a 'LAB' color to the 'XYZ' color space using the given
-- reference white illuminant.
--
-- NB: the convertible instance uses the 'd65' reference white
-- illuminant, use this function if you need to use a different
-- reference white.
labToXYZ :: CIE.LAB -> Illuminant.RefWhite -> CIE.XYZ
labToXYZ (unLAB -> ColorCoord(l,a,b)) (Illuminant.Tristimulus refWhite) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        (nx,ny,nz) = ((*) <$$$> (transformXYZ <$$$> (x,y,z))) <***> refWhite
    in CIE.mkXYZ nx ny nz

------------------------------------------------------------------------------
-- Convertible
------------------------------------------------------------------------------
instance Convertible CIE.LAB CIE.LCH where
  -- | Convert a 'CIE.LAB' color to a 'CIE.LCH' color
  safeConvert (unLAB -> ColorCoord (l,a,b)) =
    let h = calcLCHHue (atan2 b a)
        c = sqrt ((a^(2 :: Int)) + (b^(2 :: Int)))
    in Right $ CIE.mkLCH l c h

instance Convertible CIE.LAB CIE.XYZ where
  -- | Convert a 'CIE.LAB' color to a 'CIE.XYZ' color
  safeConvert lab = Right $ labToXYZ lab Illuminant.d65

instance Convertible CIE.LAB RGB where
  -- | Convert a 'CIE.LAB' color to a 256-cubed 'RGB' color
  safeConvert = convertVia (undefined :: CIE.XYZ)

instance Convertible CIE.LAB HexRGB where
  -- | Convert a 'CIE.LAB' color to a 256-cubed, 'HexRGB' encoded 'RGB' color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible RGB CIE.LAB where
  -- | Convert a 256-cubed 'RGB' color to a 'CIE.LAB' color
  safeConvert = convertVia (undefined :: CIE.XYZ)

instance Convertible RGB CIE.LCH where
  -- | Convert a 256-cubed 'RGB' color to a 'CIE.LCH' color
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible HexRGB CIE.LAB where
  -- | Convert a 'HexRGB' encoded 256-cubed 'RGB' color to a 'CIE.LAB' color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible HexRGB CIE.LCH where
  -- | Convert a 'HexRGB' encoded 256-cubed 'RGB' color to a 'CIE.LCH' color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIE.LCH CIE.LAB where
  -- | Convert a 'CIE.LCH' color to a 'CIE.LAB' color
  safeConvert (unLCH -> ColorCoord (l,c,h)) =
    let v = h * pi / 180
    in Right $ CIE.mkLAB l ((cos v)*c) ((sin v)*c)

instance Convertible CIE.LCH RGB where
  -- | Convert a 'CIE.LCH' color to a 256-cubed 'RGB' color
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible CIE.LCH HexRGB where
  -- | Convert a 'CIE.LCH' color to a 256-cubed, 'HexRGB' encoded 'RGB' color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIE.LCH CIE.XYZ where
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible CIE.XYZ RGB where
  -- | Convert a 'CIE.XYZ' color to a 256-cubed 'RGB' color
  --
  -- This function uses the default d65 illuminant matrix.
  safeConvert = Right . toRGBMatrix d65SRGB

instance Convertible CIE.XYZ HexRGB where
  -- | Convert a 'CIE.XYZ' color to a 256-cubed, 'HexRGB' encoded 'RGB' color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible CIE.XYZ CIE.LCH where
  -- | Convert a 'CIE.XYZ' color to a 'CIE.LCH' color
  safeConvert = convertVia (undefined :: CIE.LAB)

instance Convertible CIE.XYZ CIE.LAB where
  -- | Convert a 'CIE.XYZ' color to a 'CIE.LAB' color
  --
  -- This function uses the default reference white (2deg observer,
  -- d65 illuminant).
  safeConvert xyz = Right $ xyzToLAB xyz Illuminant.d65

instance Convertible HexRGB CIE.XYZ where
  -- | Convert a 'HexRGB' encoded 256-cubed 'RGB' color to the 'CIE.XYZ' color
  safeConvert = convertVia (undefined :: RGB)
