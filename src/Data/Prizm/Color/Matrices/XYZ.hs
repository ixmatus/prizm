-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.Matrices.XYZ
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Pre-calculated illuminant matrices: <http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html RGB to XYZ Matrix>.
-----------------------------------------------------------------------------
module Data.Prizm.Color.Matrices.XYZ where

import           Data.Prizm.Types

-- TODO: this should probably be a vector of triples.
d65SRGB :: XYZtoRGB
d65SRGB = XYZtoRGB [
  [3.2404542, (-1.5371385), (-0.4985314)],
  [(-0.9692660), 1.8760108, 0.0415560],
  [0.0556434, (-0.2040259), 1.0572252]]

-- TODO: this should probably be a vector of triples.
d65Adobe :: XYZtoRGB
d65Adobe = XYZtoRGB [
  [2.0413690],[(-0.5649464)],[(-0.3446944)],
  [(-0.9692660)],[1.8760108],[0.0415560],
  [0.0134474],[(-0.1183897)],[1.0154096]]
