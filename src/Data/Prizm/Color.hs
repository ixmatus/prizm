module Data.Prizm.Color where

import Control.Applicative
import Data.Prizm.Types

pct :: Integer -> Percent
pct = (/100) . fromIntegral . (max 0) . (min 100)

blend :: (CIEXYZ Double, CIEXYZ Double) -> CIEXYZ Double
blend = blendWeighted (pct 50)

-- | Blend using a weighted average for two XYZ colors.
-- 
-- Weight are applied left to right, so if a weight of 25% is
-- supplied, then the color on the left will be multiplied by 25% and
-- the second color will be multiplied by 75%.
blendWeighted :: Percent -> (CIEXYZ Double, CIEXYZ Double) -> CIEXYZ Double
blendWeighted w (a,b) =
    let a1 = (*w) <$> a
        b1 = (*(1.0 - w)) <$> b
    in (+) <$> a1 <*> b1
