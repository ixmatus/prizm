{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Types
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Types
(
-- * Generic Utility Types
  RGBtoXYZ(..)
, XYZtoRGB(..)
, Hex
, Percent
-- * CIE Color Space Types
, module Data.Prizm.Color.CIE.Types
-- * RGB Color Space Types
, module Data.Prizm.Color.RGB.Types
) where

import           Data.Prizm.Color.CIE.Types
import           Data.Prizm.Color.RGB.Types

-- | Working space matrix to convert from sRGB to CIE XYZ.
newtype RGBtoXYZ = RGBtoXYZ [[Double]]
  deriving (Eq, Ord, Show)

-- | Working space matrix to convert from CIE XYZ to sRGB.
newtype XYZtoRGB = XYZtoRGB [[Double]]
  deriving (Eq, Ord, Show)

-- | Hexadecimal encoded color code with an octothorpe prefix; e.g:
-- @#AB9D92@.
type Hex = String

-- | A percent value ranging from -100 to 100; e.g: -82%, 80%, 10%.
type Percent = Integer
