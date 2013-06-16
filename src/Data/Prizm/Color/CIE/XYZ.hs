module Data.Prizm.Color.CIE.XYZ
(
  toRGB
, toRGBMatrix
, toLAB
, toLCH
, toHex
, fromRGB
, fromHex
, fromLAB
, fromLCH
) where

import Control.Applicative

import Data.Prizm.Types

import Data.Prizm.Color.Transform
import Data.Prizm.Color.Matrices.XYZ
import Data.Prizm.Color.CIE                 (v1, v2, refWhite)

import qualified Data.Prizm.Color.SRGB      as S
import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LAB   as LB
import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LCH   as LC


-- | @transformRGB@ transform an XYZ integer to be computed against
-- the xyzToRGB matrix.
transformRGB :: Double -> Integer
transformRGB v | v > 0.0031308 = min (round ((1.055 * (v ** (1 / 2.4)) - 0.055) * 255)) 255
               | otherwise     = min (round ((12.92 * v) * 255)) 255

transformLAB :: Double -> Double
transformLAB v | v > v1    = v ** (1/3)
               | otherwise = (v2 * v) + (16 / 116)

-- | @toRGB@ convert a CIE color to an SRGB color.
-- 
-- This function uses the default d65 illuminant matrix.
toRGB :: CIEXYZ Double -> RGB Integer
toRGB = (toRGBMatrix d65SRGB)

-- | @toRGBMatrix@ convert an XYZ color to an SRGB color using a
-- provided matrix.
toRGBMatrix :: XYZtoRGB -> CIEXYZ Double -> RGB Integer
toRGBMatrix (XYZtoRGB m) (CIEXYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (transformRGB) <$> ((zipTransform t) <$> m)
    in (S.clamp) <$> RGB r g b

-- | @fromRGB@ convenience function for converting to XYZ from RGB.
fromRGB :: RGB Integer -> CIEXYZ Double
fromRGB = S.toXYZ

-- | @toHex@ convenience function for converting XYZ straight to HEX.
toHex :: CIEXYZ Double -> Hex
toHex = S.toHex . toRGB

-- | @fromHex@ convenience function for converting to XYZ from HEX.
fromHex :: Hex -> CIEXYZ Double
fromHex = S.toXYZ . S.fromHex

-- | @toLAB@ convert an XYZ color to a LAB color.
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

-- | @fromLAB@ convenience function for converting to XYZ from LAB.
fromLAB :: CIELAB Double -> CIEXYZ Double
fromLAB = LB.toXYZ

-- | @toLCH@ convenience function for converting XYZ straight to LAB.
toLCH :: CIEXYZ Double -> CIELCH Double
toLCH = LB.toLCH . toLAB

fromLCH :: CIELCH Double -> CIEXYZ Double
fromLCH = LC.toXYZ
