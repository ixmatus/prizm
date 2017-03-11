{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE.LAB
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- Transformation functions and convenience functions, some imported
-- from the other modules to ease conversion between representations.
----------------------------------------------------------------------------
module Data.Prizm.Color.CIE.LAB
(
-- * Transform to
-- ** CIE LCH or XYZ
  toLCH
, toXYZ
) where

import           Control.Applicative
import           Data.Convertible.Base
import           Data.Convertible.Utils

import           Data.Prizm.Color.CIE   (refWhite, transformXYZ)
import           Data.Prizm.Types

------------------------------------------------------------------------------
-- Convertible
------------------------------------------------------------------------------

instance Convertible CIELAB CIELCH where
  -- | Convert a 'CIELAB' to a 'CIELCH'
  safeConvert (CIELAB (CIELABp l a b)) =
    let h = transformLCH (atan2 b a)
        c = sqrt ((a^(2 :: Int)) + (b^(2 :: Int)))
    in Right $ CIELCH (CIELCHp l c hRight . (toXYZMatrix d65SRGB))

instance Convertible CIELAB CIEXYZ where
  -- | Convert a 'CIELAB' to a 'CIEXYZ'
  safeConvert (CIELAB (CIELABp l a b)) =
    let y = (l + 16) / 116
        x = a / 500 + y
        z = y - b / 200
        [nx,ny,nz] = getZipList $ ((*) <$> ZipList ((transformXYZ) <$> [x,y,z])) <*> ZipList refWhite
    in Right $ CIEXYZ (CIEXYZp nx ny nz)

instance Convertible CIELAB RGB where
  -- | Convert a 'CIELAB' to a S'RGB'
  safeConvert = convertVia (undefined :: CIEXYZ)

instance Convertible CIELAB Hex where
  -- | Convert a 'CIELAB' to an S'RGB' hexadecimal color
  safeConvert = convertVia (undefined :: RGB)

instance Convertible RGB CIELAB where
  -- | Convert a S'RGB' to a 'CIELAB'
  safeConvert = convertVia (undefined :: CIEXYZ)

instance Convertible Hex CIELAB where
  -- | Convert an S'RGB' hexadecimal color to a 'CIELAB'
  safeConvert = convertVia (undefined :: RGB)

transformLCH :: Double -> Double
transformLCH v | v > 0      = (v / pi) * 180
               | otherwise  = 360 - ((abs v) / pi) * 180
