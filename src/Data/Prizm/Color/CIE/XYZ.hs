module Data.Prizm.Color.CIE.XYZ
(
-- * Transform to
-- ** RGB or Hex
  toRGB
, toRGBMatrix
, toHex

-- ** CIE LAB or LCH
, toLAB
, toLCH

-- * Transform from
-- ** RGB or Hex
, fromRGB
, fromHex

-- ** CIE LAB or LCH
, fromLAB
, fromLCH
) where

import           Control.Applicative

import           Data.Prizm.Types

import           Data.Prizm.Color.CIE          (refWhite, v1, v2)
import           Data.Prizm.Color.Matrices.XYZ
import           Data.Prizm.Color.Transform

import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LAB      as LB
import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LCH      as LC
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
-- Transform to
------------------------------------------------------------------------------

-- | Convert a CIE color to an SRGB color.
--
-- This function uses the default d65 illuminant matrix.
toRGB :: CIEXYZ Double -> RGB Integer
toRGB = (toRGBMatrix d65SRGB)

-- | Convert an XYZ color to an SRGB color.
--
-- 'XYZtoRGB' is the pre-calculated illuminant matrix, it is
-- preferable to use 'toRG' as it uses the most "common" one.
toRGBMatrix :: XYZtoRGB -> CIEXYZ Double -> RGB Integer
toRGBMatrix (XYZtoRGB m) (CIEXYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (transformRGB) <$> ((zipTransform t) <$> m)
    in (S.clamp) <$> RGB r g b

-- | Convenience function to convert XYZ to HEX.
toHex :: CIEXYZ Double -> Hex
toHex = S.toHex . toRGB

-- | Convert an XYZ color to a LAB color.
--
-- This function uses the default reference white (2deg observer, d65
-- illuminant).
toLAB :: CIEXYZ Double -> CIELAB Double
toLAB (CIEXYZ x y z) =
    let v = getZipList $ ZipList ((/) <$> [x,y,z]) <*> ZipList refWhite
        [tx,ty,tz] = (transformLAB) <$> v
        l = (116 * ty) - 16
        a = 500 * (tx - ty)
        b = 200 * (ty - tz)
    in CIELAB l a b

-- | Convenience function to convert XYZ to LAB.
toLCH :: CIEXYZ Double -> CIELCH Double
toLCH = LB.toLCH . toLAB

------------------------------------------------------------------------------
-- Transform from
------------------------------------------------------------------------------

-- | Convenience function to convert RGB to XYZ.
fromRGB :: RGB Integer -> CIEXYZ Double
fromRGB = S.toXYZ

-- | Convenience function to convert HEX to XYZ.
fromHex :: Hex -> CIEXYZ Double
fromHex = S.toXYZ . S.fromHex

-- | Convenience function to convert LAB to XYZ.
fromLAB :: CIELAB Double -> CIEXYZ Double
fromLAB = LB.toXYZ

-- | Convenience function to convert LCH to XYZ.
fromLCH :: CIELCH Double -> CIEXYZ Double
fromLCH = LC.toXYZ
