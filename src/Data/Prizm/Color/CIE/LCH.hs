{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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
module Data.Prizm.Color.CIE.LCH where

import           Data.Convertible.Base
import           Data.Convertible.Utils

import           Data.Prizm.Types

------------------------------------------------------------------------------
-- Convertible instances
------------------------------------------------------------------------------

instance Convertible CIELCH CIELAB where
  -- | Convert a 'CIELCH' to a 'CIELAB'
  safeConvert (CIELCH (CIELCHp l c h)) =
    let v = h * pi / 180
    in Right $ CIELAB (CIELABp l ((cos v)*c) ((sin v)*c))

instance Convertible CIELCH RGB where
  -- | Convert a 'CIELCH' to a S'RGB'
  safeConvert = convertVia (undefined :: CIEXYZ)

instance Convertible CIELCH CIEXYZ where
  safeConvert = convertVia (undefined :: CIELAB)

-- -- | Convert from LCH to RGB.
-- toRGB :: CIELCH -> RGB
-- toRGB = X.toRGB . toXYZ

-- -- | Convert from LCH to Hex.
-- toHex :: CIELCH -> Hex
-- toHex = LB.toHex . toLAB





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
