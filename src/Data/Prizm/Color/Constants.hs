-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.Constants
-- Copyright   :  (C) 2017 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
----------------------------------------------------------------------------
module Data.Prizm.Color.Constants where

-- | Exact rational of the "0.008856" value.
ζ :: Double
ζ = (6/29) ** 3

-- | Exact rational of the "7.787" value.
ξ :: Double
ξ = 1/3 * ((29/6) ** 2)
