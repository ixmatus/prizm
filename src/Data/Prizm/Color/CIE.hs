module Data.Prizm.Color.CIE
(
  toRGB
, toRGBMatrix
, toLAB
, toXYZ
) where

import Control.Applicative

import Data.Prizm.Types
import Data.Prizm.Color.Transform
import Data.Prizm.Color.Matrices.XYZ

-- 2deg observer, d65 illuminant
-- [x,y,z]
refWhite :: [Double]
refWhite = [95.047, 100.000, 108.883]

-- | @transformRGB@ transform an XYZ integer to be computed against
-- the xyzToRGB matrix.
transformRGB :: Double -> Integer
transformRGB v | v > 0.0031308 = min (round ((1.055 * (v ** (1 / 2.4)) - 0.055) * 255)) 255
               | otherwise     = min (round ((12.92 * v) * 255)) 255

transformLAB :: Double -> Double
transformLAB v | v > 0.008856 = v ** (1/3)
               | otherwise    = (7.787 * v) + (16 / 116)

transformXYZ :: Double -> Double
transformXYZ v | cv > 0.008856 = cv
               | otherwise = (v - 16 / 116) / 7.787
    where cv = v**3

-- | @toRGB@ convert a CIE color to an SRGB color.
-- 
-- Once I've implemented CIE L*a*b -> XYZ and vice-versa functions
-- then I'll introduce the type exhaustively here to handle any CIE
-- color -> SRGB conversion.
toRGB :: CIE -> RGB
toRGB = (toRGBMatrix d65SRGB)

toRGBMatrix :: XYZtoRGB -> CIE -> RGB
toRGBMatrix m (LAB l a b) = toRGBMatrix m (toXYZ (LAB l a b))
toRGBMatrix m (XYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (transformRGB) <$> ((zipTransform t) <$> m)
    in RGB r g b

toLAB :: CIE -> CIE
toLAB (LAB l a b) = LAB l a b
toLAB (XYZ x y z) =
    let v = getZipList $ ZipList ((/) <$> [x,y,z]) <*> ZipList refWhite
        [tx,ty,tz] = (transformLAB) <$> v
        l = (116 * ty) - 16
        a = 500 * (tx - ty)
        b = 200 * (ty - tz)
    in LAB l a b

toXYZ :: CIE -> CIE
toXYZ (XYZ x y z) = XYZ x y z
toXYZ (LAB l a b) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        [nx,ny,nz] = getZipList $ ((*) <$> ZipList ((transformXYZ) <$> [x,y,z])) <*> ZipList refWhite
    in XYZ nx ny nz
    
