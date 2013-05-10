module Data.Prizm.Color.SRGB
(
  toXYZ
) where

import Data.Prizm.Types
import Data.Prizm.Color.Transform

import Control.Applicative

matrix :: [[Double]]
matrix = [
  [0.4124564, 0.3575761, 0.1804375],
  [0.2126729, 0.7151522, 0.0721750],
  [0.0193339, 0.1191920, 0.9503041]]

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
