module Prizm.Color.CIE
(
  toRGB
) where

import Prizm.Types
import Prizm.Color.Transform

import Control.Applicative

matrix :: [[Double]]
matrix = [
  [3.2406, (-1.5372), (-0.4986)],
  [(-0.9689), 1.8758, 0.0415],
  [0.0557, (-0.2040), 1.0570]]

-- | @transform@ transform an XYZ integer to be computed against
-- the xyzToRGB matrix.
transform :: Double -> Integer
transform v | v > 0.0031308 = min (truncate ((1.055 * (v ** (1 / 2.4)) - 0.055) * 255)) 255
            | otherwise     = min (truncate ((12.92 * v) * 255)) 255

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
