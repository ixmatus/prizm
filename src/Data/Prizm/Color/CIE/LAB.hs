module Data.Prizm.Color.CIE.LAB
(
  toLCH
, toXYZ
, toRGB
, toHex
, fromXYZ
, fromRGB
, fromHex
, fromLCH
) where

import Control.Applicative

import Data.Prizm.Types
import Data.Prizm.Color.CIE             (refWhite, transformXYZ)

import qualified Data.Prizm.Color.SRGB      as S
import qualified Data.Prizm.Color.CIE.XYZ   as X
import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LCH   as LC

transformLCH :: Double -> Double
transformLCH v | v > 0      = (v / pi) * 180
               | otherwise  = 360 - ((abs v) / pi) * 180

-- | @toLCH@ convert a LAB color to the LCH representation.
toLCH :: CIELAB Double -> CIELCH Double
toLCH (CIELAB l a b) =
    let h = transformLCH (atan2 b a)
        c = sqrt ((a^2) + (b^2))
    in CIELCH l c h

-- | @fromLCH@ convert a LCH color to the LAB representation.
fromLCH :: CIELCH Double -> CIELAB Double
fromLCH = LC.toLAB

-- | @toXYZ@ convert a LAB color to the XYZ representation.
toXYZ :: CIELAB Double -> CIEXYZ Double
toXYZ (CIELAB l a b) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        [nx,ny,nz] = getZipList $ ((*) <$> ZipList ((transformXYZ) <$> [x,y,z])) <*> ZipList refWhite
    in CIEXYZ nx ny nz

-- | @fromXYZ@ convert an XYZ color straight to LAB.
fromXYZ :: CIEXYZ Double -> CIELAB Double
fromXYZ = X.toLAB

-- | @toRGB@ convenience function for converting straight to RGB.
toRGB :: CIELAB Double -> RGB Integer
toRGB = X.toRGB . toXYZ

-- | @fromRGB@ convenience function for converting to LAB from RGB.
fromRGB :: RGB Integer -> CIELAB Double
fromRGB = X.toLAB . S.toXYZ

-- | @toHex@ convenience function for converting straight to HEX.
toHex :: CIELAB Double -> Hex
toHex = X.toHex . toXYZ

-- | @fromHex@ convenience function for converting to LAB from HEX.
fromHex :: Hex -> CIELAB Double
fromHex = X.toLAB . X.fromHex
