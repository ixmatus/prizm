module Data.Prizm.Color.CIE
(
  v1
, v2
, refWhite
, transformXYZ
) where

-- | exact rational of the "0.008856" value.
v1 :: Double
v1 = (6/29) ** 3

-- | exact rational of the "7.787" value.
v2 :: Double
v2 = 1/3 * ((29/6) ** 2)

-- 2deg observer, d65 illuminant
-- [x,y,z]
refWhite :: [Double]
refWhite = [95.047, 100.000, 108.883]

transformXYZ :: Double -> Double
transformXYZ v | cv > v1   = cv
               | otherwise = (v - 16 / 116) / v2
    where cv = v**3
