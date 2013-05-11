module Data.Prizm.Color.CIE
(
  toRGB
, toRGBMatrix
) where

import Control.Applicative

import Data.Prizm.Types
import Data.Prizm.Color.Transform

import Data.Prizm.Color.Matrices.XYZ

-- | @transform@ transform an XYZ integer to be computed against
-- the xyzToRGB matrix.
transform :: Double -> Integer
transform v | v > 0.0031308 = min (round ((1.055 * (v ** (1 / 2.4)) - 0.055) * 255)) 255
            | otherwise     = min (round ((12.92 * v) * 255)) 255

-- | @toRGB@ convert a CIE color to an SRGB color.
-- 
-- Once I've implemented CIE L*a*b -> XYZ and vice-versa functions
-- then I'll introduce the type exhaustively here to handle any CIE
-- color -> SRGB conversion.
toRGB :: CIE -> SRGB
--toRGB (LAB _ _ _) = Nothing
toRGB = (toRGBMatrix d65SRGB)

toRGBMatrix :: XYZtoRGB -> CIE -> SRGB
toRGBMatrix m (XYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (transform) <$> ((zipTransform t) <$> m)
    in SRGB r g b
