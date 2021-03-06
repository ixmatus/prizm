-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE.Matrices.RGB
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Pre-calculated illuminant matrices: <http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html RGB to XYZ Matrix>.
-----------------------------------------------------------------------------
module Data.Prizm.Color.CIE.Matrices.RGB where

-- | Working space matrix to convert from sRGB to CIE XYZ.
newtype RGBtoXYZ = Matrix [[Double]]
  deriving (Eq, Ord, Show)

-- TODO: this should probably be a vector of triples.
d65SRGB :: RGBtoXYZ
d65SRGB = Matrix [
  [0.4124564, 0.3575761, 0.1804375],
  [0.2126729, 0.7151522, 0.0721750],
  [0.0193339, 0.1191920, 0.9503041]]

-- TODO: this should probably be a vector of triples.
d65Adobe :: RGBtoXYZ
d65Adobe = Matrix [
  [0.5767309],[0.1855540],[0.1881852],
  [0.2973769],[0.6273491],[0.0752741],
  [0.0270343],[0.0706872],[0.9911085]]
