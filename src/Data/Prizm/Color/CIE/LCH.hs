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
-- ** CIE LAB or XYZ
  toLAB
) where

import           Data.Prizm.Types

------------------------------------------------------------------------------
-- Transform to
------------------------------------------------------------------------------

-- -- | Convert from LCH to RGB.
-- toRGB :: CIELCH -> RGB
-- toRGB = X.toRGB . toXYZ

-- -- | Convert from LCH to Hex.
-- toHex :: CIELCH -> Hex
-- toHex = LB.toHex . toLAB

-- | Convert an LCH color to LAB.
toLAB :: CIELCH -> CIELAB
toLAB (CIELCH (CIELCHp l c h)) =
    let v = h * pi / 180
    in CIELAB $ CIELABp l ((cos v)*c) ((sin v)*c)

-- -- | Convert from LCH to XYZ.
-- toXYZ :: CIELCH -> CIEXYZ
-- toXYZ = LB.toXYZ . toLAB

------------------------------------------------------------------------------
-- Transform from
------------------------------------------------------------------------------

-- -- | Convenience function to convert from RGB to LCH.
-- fromRGB :: RGB -> CIELCH
-- fromRGB = fromLAB . LB.fromRGB

-- -- | Convenience function to convert from Hex to LCH.
-- fromHex :: Hex -> CIELCH
-- fromHex = LB.toLCH . LB.fromHex

-- -- | Convenience function to convert from LAB to LCH.
-- fromLAB :: CIELAB -> CIELCH
-- fromLAB = LB.toLCH

-- -- | Convert from XYZ to LCH.
-- fromXYZ :: CIEXYZ -> CIELCH
-- fromXYZ = X.toLCH
