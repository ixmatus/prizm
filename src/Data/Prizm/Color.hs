module Data.Prizm.Color where

import Control.Applicative
import Data.Prizm.Types
import Data.Prizm.Color.CIE as C

pct :: Integer -> Double
pct = (/100) . fromIntegral . (max 0) . (min 100)

pctClamp :: Integer -> Integer
pctClamp i = max (min i 100) 0

-- | Blend two colors using a 50% weighted average.
blend :: (CIEXYZ Double, CIEXYZ Double) -> CIEXYZ Double
blend = blendWeighted 50

-- | Shade a color by blending it using a weight and the color black.
shade :: CIEXYZ Double -> Percent -> CIEXYZ Double
shade c w = blendWeighted (pctClamp w) (CIEXYZ 0.0 0.0 0.0, c)

-- | Tint a color by blending it using a weight and the color white.
tint :: CIEXYZ Double -> Percent -> CIEXYZ Double
tint c w = blendWeighted (pctClamp w) ((CIEXYZ 95.047 100.0 108.883), c)

-- | Darken a color by converting it to CIE L*a*b* first, multiplying
-- it by the weight, and then subtracting that value from the original
-- L* value and converting the whole back to XYZ.
darken :: CIEXYZ Double -> Percent -> CIEXYZ Double
darken c w =
    let (CIELAB l a b) = C.toLAB c
        l' = l - (l*(pct (pctClamp w)))
    in C.toXYZ (CIELAB l' a b)

-- | Lighten a color by converting it to CIE L*a*b* first, multiplying
-- it by the weight, and then adding that value to the original L*
-- value and converting the whole back to XYZ.
lighten :: CIEXYZ Double -> Percent -> CIEXYZ Double
lighten c w =
    let (CIELAB l a b) = C.toLAB c
        l' = l + (l*(pct (pctClamp w)))
    in C.toXYZ (CIELAB l' a b)

-- | Blend using a weighted average for two XYZ colors.
-- 
-- Weight are applied left to right, so if a weight of 25% is
-- supplied, then the color on the left will be multiplied by 25% and
-- the second color will be multiplied by 75%.
blendWeighted :: Percent -> (CIEXYZ Double, CIEXYZ Double) -> CIEXYZ Double
blendWeighted w (a,b) =
    let w' = (pct (pctClamp w))
        a1 = (*w') <$> a
        b1 = (*(1.0 - w')) <$> b
    in (+) <$> a1 <*> b1
