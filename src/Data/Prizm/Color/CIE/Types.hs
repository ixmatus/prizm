{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE.Types
-- Copyright   :  (C) 2017 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Color.CIE.Types where

import           Data.Prizm.Types

-- | A color in the @CIE XYZ@ color space.
newtype XYZ = XYZ { unXYZ :: ColorCoord Double }
  deriving (Eq, Ord, Show)

-- | A color in the @CIE L*a*b*@ color space.
newtype LAB = LAB { unLAB :: ColorCoord Double }
  deriving (Eq, Ord, Show)

-- | A color in the @CIE L*C*h(uv)@ color space.
newtype LCH = LCH { unLCH :: ColorCoord Double }
  deriving (Eq, Ord, Show)

-- | Produce a CIE XYZ color.
mkXYZ :: Double -- ^ @X@ color point
      -> Double -- ^ @Y@ color point
      -> Double -- ^ @Z@ color point
      -> XYZ
mkXYZ x y z = XYZ (ColorCoord (x,y,z))

-- | Produce a CIE LAB color.
mkLAB :: Double -- ^ @L@  color point
      -> Double -- ^ @*a@ color point
      -> Double -- ^ @*b@ color point
      -> LAB
mkLAB l a b = LAB (ColorCoord (l,a,b))

-- | Produce a CIE LCH color.
mkLCH :: Double -- ^ @L@  color point
      -> Double -- ^ @*c@ color point
      -> Double -- ^ @*h@ color point
      -> LCH
mkLCH l c h = LCH (ColorCoord (l,c,h))
