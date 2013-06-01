module Data.Prizm.Color where

import Data.Prizm.Types
import Data.Prizm.Color.CIE as C

pct :: Integer -> Double
pct = (/100) . fromIntegral . (max 0) . (min 100)

pctClamp :: Integer -> Integer
pctClamp i = max (min i 100) 0

clamp :: Double -> Double -> Double
clamp i clmp = max (min i clmp) 0.0

-- | Blend two colors using a 50% weighted average.
blend :: (CIELCH Double, CIELCH Double) -> CIELCH Double
blend = blendWeighted 50

-- | Shade a color by blending it using a weight and the color black.
shade :: CIELCH Double -> Percent -> CIELCH Double
shade c w = blendWeighted (pctClamp w) (CIELCH 0.0 0.0 360.0, c)

-- | Tint a color by blending it using a weight and the color white.
tint :: CIELCH Double -> Percent -> CIELCH Double
tint c w = blendWeighted (pctClamp w) (CIELCH 100.0 0.0 360.0, c)

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

-- | Blend using a weighted average for two LCh colors.
-- 
-- Weight is applied left to right, so if a weight of 25% is supplied,
-- then the color on the left will be multiplied by 25% and the second
-- color will be multiplied by 75%.
-- 
-- CIE L*Ch is used because the interpolation between the colors is
-- more accurate than L*ab, XYZ, and sRGB color spaces.
blendWeighted' :: Percent -> (CIELCH Double, CIELCH Double) -> CIELCH Double
blendWeighted' w ((CIELCH l c h),(CIELCH l' c' h')) =
    let w'  = (pct (pctClamp w))
        w'' = 1.0 - w'
        nl  = clamp ((l*w') + (l'*w'')) 100
        nc  = (c*w') + (c'*w'')
        nh  = (h*w') + (h'*w'')
    in CIELCH nl nc nh

shortestPath :: Double -> Double
shortestPath h | h > 180    = h - 360
               | h < (-180) = h + 360
               | otherwise  = h

blendWeighted :: Percent -> (CIELCH Double, CIELCH Double) -> CIELCH Double
blendWeighted w ((CIELCH l c h),(CIELCH l' c' h')) =
    let w'  = (pct (pctClamp w))
        nl  = if l > l' then l - l' else l' - l
        nc  = c - c'
        nh  = h - h'
        zh  = shortestPath nh
    in CIELCH (clamp (l + nl * w') 100) (c + nc * w') (clamp (h + zh * w') 360)
