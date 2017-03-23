-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE.Matrices.XYZ
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Pre-calculated illuminant matrices: <http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html RGB to XYZ Matrix>.
-----------------------------------------------------------------------------
module Data.Prizm.Color.CIE.Matrices.XYZ where

-- | Working space matrix to convert from CIE XYZ to sRGB.
newtype XYZtoRGB = Matrix [[Double]]
  deriving (Eq, Ord, Show)


d65SRGB :: XYZtoRGB
d65SRGB = Matrix [
  [3.2404542, (-1.5371385), (-0.4985314)],
  [(-0.9692660), 1.8760108, 0.0415560],
  [0.0556434, (-0.2040259), 1.0572252]]

-- TODO: this should probably be a vector of triples.
d65Adobe :: XYZtoRGB
d65Adobe = Matrix [
  [2.0413690],[(-0.5649464)],[(-0.3446944)],
  [(-0.9692660)],[1.8760108],[0.0415560],
  [0.0134474],[(-0.1183897)],[1.0154096]]
