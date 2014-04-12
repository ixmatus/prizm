module Data.Prizm.Color.CIE.LCH
( toRGB
, toLAB
, toXYZ
, toHex
, fromRGB
, fromHex
, fromLAB
, fromXYZ
) where

import           Data.Prizm.Types

import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LAB as LB
import qualified Data.Prizm.Color.CIE.XYZ as X

-- | @toLAB@ convert an LCH color to a LAB representation.
toLAB :: CIELCH Double -> CIELAB Double
toLAB (CIELCH l c h) =
    let v = h * pi / 180
    in CIELAB l ((cos v)*c) ((sin v)*c)

-- | @fromLAB@ convenience function for converting from LAB to LCH.
fromLAB :: CIELAB Double -> CIELCH Double
fromLAB = LB.toLCH

-- | @toXYZ@ convert from LCH to XYZ.
toXYZ :: CIELCH Double -> CIEXYZ Double
toXYZ = LB.toXYZ . toLAB

-- | @fromXYZ@ convert from XYZ to LCH.
fromXYZ :: CIEXYZ Double -> CIELCH Double
fromXYZ = X.toLCH

-- | @toRGB@ convert from LCH to RGB.
toRGB :: CIELCH Double -> RGB Integer
toRGB = X.toRGB . toXYZ

-- | @fromRGB@ convert from RGB to LCH.
fromRGB :: RGB Integer -> CIELCH Double
fromRGB = fromLAB . LB.fromRGB

toHex :: CIELCH Double -> Hex
toHex = LB.toHex . toLAB

fromHex :: Hex -> CIELCH Double
fromHex = LB.toLCH . LB.fromHex
