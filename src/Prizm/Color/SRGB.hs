module Prizm.Color.SRGB
(
  toXYZ
) where

import Prizm.Types
import Prizm.Color.Transform

import Control.Applicative

matrix :: [[Double]]
matrix = [
  [0.4124, 0.3576, 0.1805],
  [0.2126, 0.7152, 0.0722],
  [0.0193, 0.1192, 0.9505]]

-- | @rgbTransform@ transform an RGB integer to be computed against
-- the rgbToXYZ matrix.
transform :: Integer -> Double
transform v | dv > 0.04045 = (((dv + 0.055) / ap) ** 2.4) * 100
               | otherwise    = (dv / 12.92) * 100
    where dv = fromIntegral v / 255
          ap = 1.0 + 0.055

-- | @toXYZ@ convert an sRGB value to a CIE XYZ value.
toXYZ :: SRGB -> CIE
toXYZ (SRGB r g b) =
    let t = ZipList (transform <$> [r,g,b])
        [x,y,z] = (zipTransform t) <$> matrix
    in XYZ x y z