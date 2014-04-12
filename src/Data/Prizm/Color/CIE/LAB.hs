module Data.Prizm.Color.CIE.LAB
(
-- * Transform to
-- ** RGB or Hex
  toRGB
, toHex
-- ** CIE LCH or XYZ
, toLCH
, toXYZ

-- * Transform from
-- ** RGB or Hex

, fromRGB
, fromHex

-- ** CIE XYZ or LCH
, fromXYZ
, fromLCH
) where

import           Control.Applicative

import           Data.Prizm.Color.CIE     (refWhite, transformXYZ)
import           Data.Prizm.Types

import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LCH as LC
import qualified Data.Prizm.Color.CIE.XYZ as X
import qualified Data.Prizm.Color.SRGB    as S

------------------------------------------------------------------------------
-- Transform to
------------------------------------------------------------------------------

-- | Convenience function to convert LAB to RGB.
toRGB :: CIELAB Double -> RGB Integer
toRGB = X.toRGB . toXYZ

-- | Convenience function to convert LAB to HEX.
toHex :: CIELAB Double -> Hex
toHex = X.toHex . toXYZ

-- | Convert a LAB color to the LCH representation.
toLCH :: CIELAB Double -> CIELCH Double
toLCH (CIELAB l a b) =
    let h = transformLCH (atan2 b a)
        c = sqrt ((a^(2 :: Int)) + (b^(2 :: Int)))
    in CIELCH l c h

-- | Convert a LAB color to the XYZ representation.
toXYZ :: CIELAB Double -> CIEXYZ Double
toXYZ (CIELAB l a b) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        [nx,ny,nz] = getZipList $ ((*) <$> ZipList ((transformXYZ) <$> [x,y,z])) <*> ZipList refWhite
    in CIEXYZ nx ny nz

------------------------------------------------------------------------------
-- Transform from
------------------------------------------------------------------------------

-- | Convenience function to convert RGB to LAB.
fromRGB :: RGB Integer -> CIELAB Double
fromRGB = X.toLAB . S.toXYZ

-- | Convenience function to convert HEX to LAB.
fromHex :: Hex -> CIELAB Double
fromHex = X.toLAB . X.fromHex

-- | Convert an LCH color to the LAB representation.
fromLCH :: CIELCH Double -> CIELAB Double
fromLCH = LC.toLAB

-- | Convert an XYZ color to LAB.
fromXYZ :: CIEXYZ Double -> CIELAB Double
fromXYZ = X.toLAB

transformLCH :: Double -> Double
transformLCH v | v > 0      = (v / pi) * 180
               | otherwise  = 360 - ((abs v) / pi) * 180
