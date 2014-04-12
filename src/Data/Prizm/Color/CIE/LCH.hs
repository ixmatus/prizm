-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE.LCH
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- Transformation functions and convenience functions, some imported
-- from the other modules to ease conversion between representations.
----------------------------------------------------------------------------
module Data.Prizm.Color.CIE.LCH
(
-- * Transform to
-- ** RGB or Hex

  toRGB
, toHex

-- ** CIE LAB or XYZ
, toLAB
, toXYZ

-- * Transform from
-- ** RGB or Hex
, fromRGB
, fromHex

-- ** CIE LAB or XYZ
, fromLAB
, fromXYZ
) where

import           Data.Prizm.Types

import {-# SOURCE #-} qualified Data.Prizm.Color.CIE.LAB as LB
import qualified Data.Prizm.Color.CIE.XYZ as X

------------------------------------------------------------------------------
-- Transform to
------------------------------------------------------------------------------

-- | Convert from LCH to RGB.
toRGB :: CIELCH Double -> RGB Integer
toRGB = X.toRGB . toXYZ

-- | Convert from LCH to Hex.
toHex :: CIELCH Double -> Hex
toHex = LB.toHex . toLAB

-- | Convert an LCH color to LAB.
toLAB :: CIELCH Double -> CIELAB Double
toLAB (CIELCH l c h) =
    let v = h * pi / 180
    in CIELAB l ((cos v)*c) ((sin v)*c)

-- | Convert from LCH to XYZ.
toXYZ :: CIELCH Double -> CIEXYZ Double
toXYZ = LB.toXYZ . toLAB

------------------------------------------------------------------------------
-- Transform from
------------------------------------------------------------------------------

-- | Convenience function to convert from RGB to LCH.
fromRGB :: RGB Integer -> CIELCH Double
fromRGB = fromLAB . LB.fromRGB

-- | Convenience function to convert from Hex to LCH.
fromHex :: Hex -> CIELCH Double
fromHex = LB.toLCH . LB.fromHex

-- | Convenience function to convert from LAB to LCH.
fromLAB :: CIELAB Double -> CIELCH Double
fromLAB = LB.toLCH

-- | Convert from XYZ to LCH.
fromXYZ :: CIEXYZ Double -> CIELCH Double
fromXYZ = X.toLCH
