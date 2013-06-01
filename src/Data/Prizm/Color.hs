module Data.Prizm.Color where

import Control.Applicative

import Data.Prizm.Types

pct :: Integer -> Double
pct = (/100) . fromIntegral . (max 0) . (min 100)

pctClamp :: Integer -> Integer
pctClamp i = max (min i 100) 0

clamp :: Double -> Double -> Double
clamp i clmp = max (min i clmp) 0.0

-- | Blend two colors using an interpolation value of 50%.
(<|>) :: (CIELCH Double, CIELCH Double) -> CIELCH Double
(<|>) = interpolate 50

-- | Shade a color by blending it using a weight and the color black.
shade :: CIELCH Double -> Percent -> CIELCH Double
shade c w = interpolate (pctClamp w) (c, CIELCH 0.0 0.0 360.0)

-- | Tint a color by blending it using a weight and the color white.
tint :: CIELCH Double -> Percent -> CIELCH Double
tint c w = interpolate (pctClamp w) (c, CIELCH 100.0 0.0 360.0)

-- | Darken a color by converting it to CIE L*a*b* first, multiplying
-- it by the weight, and then subtracting that value from the original
-- L* value and converting the whole back to XYZ.
darken :: CIELCH Double -> Percent -> CIELCH Double
darken (CIELCH l c h) w = (CIELCH (l - (l*(pct (pctClamp w)))) c h)

-- | Lighten a color by converting it to CIE L*a*b* first, multiplying
-- it by the weight, and then adding that value to the original L*
-- value and converting the whole back to XYZ.
lighten :: CIELCH Double -> Percent -> CIELCH Double
lighten (CIELCH l c h) w = (CIELCH (l + (l*(pct (pctClamp w)))) c h)

-- | Interpolate two colors
-- 
-- Weight is applied left to right, so if a weight of 25% is supplied,
-- then the color on the left will be multiplied by 25% and the second
-- color will be multiplied by 75%.
-- 
-- CIE L*Ch is used because the interpolation between the colors is
-- more accurate than L*ab, XYZ, and sRGB color spaces.
interpolate :: Percent -> (CIELCH Double, CIELCH Double) -> CIELCH Double
interpolate w (a,b) =
    let w' = (pct (pctClamp w))
        (CIELCH l c h) = (-) <$> b <*> a
        a' = (*w') <$> (CIELCH l c (shortestPath h))
    in (+) <$> a' <*> a

shortestPath :: Double -> Double
shortestPath h | h > 180    = h - 360
               | h < (-180) = h + 360
               | otherwise  = h
