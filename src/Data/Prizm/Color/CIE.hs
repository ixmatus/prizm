module Data.Prizm.Color.CIE
(
  toRGB
) where

import Data.Prizm.Types
import Data.Prizm.Color.Transform

import Control.Applicative

matrix :: [[Double]]
matrix = [
  [3.2404542, (-1.5371385), (-0.4985314)],
  [(-0.9692660), 1.8760108, 0.0415560],
  [0.0556434, (-0.2040259), 1.0572252]]

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
toRGB (XYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (transform) <$> ((zipTransform t) <$> matrix)
    in SRGB r g b
