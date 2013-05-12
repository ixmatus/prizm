module Data.Prizm.Color.SRGB
(
  toXYZ
, toXYZMatrix
) where

import Data.Prizm.Types
import Data.Prizm.Color.Transform
import Data.Prizm.Color.Matrices.RGB

import Control.Applicative

-- | @rgbTransform@ transform an RGB integer to be computed against
-- a matrix.
transform :: Integer -> Double
transform v | dv > 0.04045 = (((dv + 0.055) / ap) ** 2.4) * 100
            | otherwise    = (dv / 12.92) * 100
    where dv = fromIntegral v / 255
          ap = 1.0 + 0.055

-- | @toXYZ@ convert an sRGB value to a CIE XYZ value.
toXYZ :: RGB -> CIE
toXYZ = (toXYZMatrix d65SRGB)

toXYZMatrix :: RGBtoXYZ -> RGB -> CIE
toXYZMatrix m (RGB r g b) =
    let t = ZipList (transform <$> [r,g,b])
        [x,y,z] = (roundN 3) <$> ((zipTransform t) <$> m)
    in XYZ x y z
