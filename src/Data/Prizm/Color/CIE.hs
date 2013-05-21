module Data.Prizm.Color.CIE
(
  toRGB
, toRGBMatrix
, toLAB
, toXYZ
) where

import Control.Applicative

import Data.Prizm.Types
import Data.Prizm.Color.SRGB            (clamp)
import Data.Prizm.Color.Transform
import Data.Prizm.Color.Matrices.XYZ

-- 2deg observer, d65 illuminant
-- [x,y,z]
refWhite :: [Double]
refWhite = [95.047, 100.000, 108.883]

-- | exact rational of the "0.008856" value.
v1 :: Double
v1 = (6/29) ** 3

-- | exact rational of the "7.787" value.
v2 :: Double
v2 = 1/3 * ((29/6) ** 2)

-- | @transformRGB@ transform an XYZ integer to be computed against
-- the xyzToRGB matrix.
transformRGB :: Double -> Integer
transformRGB v | v > 0.0031308 = min (round ((1.055 * (v ** (1 / 2.4)) - 0.055) * 255)) 255
               | otherwise     = min (round ((12.92 * v) * 255)) 255

transformLAB :: Double -> Double
transformLAB v | v > v1 = v ** (1/3)
               | otherwise    = (v2 * v) + (16 / 116)

transformXYZ :: Double -> Double
transformXYZ v | cv > v1 = cv
               | otherwise     = (v - 16 / 116) / v2
    where cv = v**3

-- | @toRGB@ convert a CIE color to an SRGB color.
-- 
-- Once I've implemented CIE L*a*b -> XYZ and vice-versa functions
-- then I'll introduce the type exhaustively here to handle any CIE
-- color -> SRGB conversion.
toRGB :: CIEXYZ Double -> RGB Integer
toRGB = (toRGBMatrix d65SRGB)

toRGBMatrix :: XYZtoRGB -> CIEXYZ Double -> RGB Integer
toRGBMatrix m (CIEXYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (transformRGB) <$> ((zipTransform t) <$> m)
    in (clamp) <$> RGB r g b

toLAB :: CIEXYZ Double -> CIELAB Double
toLAB (CIEXYZ x y z) =
    let v = getZipList $ ZipList ((/) <$> [x,y,z]) <*> ZipList refWhite
        [tx,ty,tz] = (transformLAB) <$> v
        l = (116 * ty) - 16
        a = 500 * (tx - ty)
        b = 200 * (ty - tz)
    in CIELAB l a b

toXYZ :: CIELAB Double -> CIEXYZ Double
toXYZ (CIELAB l a b) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        [nx,ny,nz] = getZipList $ ((*) <$> ZipList ((transformXYZ) <$> [x,y,z])) <*> ZipList refWhite
    in CIEXYZ nx ny nz
    
