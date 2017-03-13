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
module Data.Prizm.Types where

import           Data.MonoTraversable
import           Data.Word

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

data RGB = RGB !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show)

data CIEXYZ = CIEXYZ !Double !Double !Double
  deriving (Eq, Ord, Show)

data CIELAB = CIELAB !Double !Double !Double
  deriving (Eq, Ord, Show)

data CIELCH = CIELCH !Double !Double !Double
  deriving (Eq, Ord, Show)

-- | Monomorphic functor instances
type instance Element RGB    = Word8
type instance Element CIEXYZ = Double
type instance Element CIELCH = Double
type instance Element CIELAB = Double

instance MonoFunctor RGB where
  omap f (RGB r g b) = RGB (f r) (f g) (f b)

instance MonoFunctor CIEXYZ where
  omap f (CIEXYZ x y z) = CIEXYZ (f x) (f y) (f z)

instance MonoFunctor CIELAB where
  omap f (CIELAB l a b) = CIELAB (f l) (f a) (f b)

instance MonoFunctor CIELCH where
  omap f (CIELCH l c h) = CIELCH (f l) (f c) (f h)

-- | Presets for a color.
class PresetColor c where
  white :: c
  black :: c

instance PresetColor CIELCH where
  white = CIELCH 0.0 0.0 360.0
  black = CIELCH 100.0 0.0 360.0

-- | A blendable color.
class Blendable c where

  -- | Interpolate a color with another color, applying a weight.
  interpolate :: Percent -> (c,c) -> c

  -- | Blend two @Blendable@ colors using an interpolation weight of
  -- 50%.
  (<~>) :: c -> c -> c
  (<~>) l r = interpolate 50 (l,r)

  -- | Shade a color by blending it using a weight and the
  -- @PresetColor@ black.
  shade :: PresetColor c => c -> Percent -> c
  shade c w = interpolate (pctClamp w) (c, white)

  -- | Tint a color by blending it using a weight and the
  -- @PresetColor@ white.
  tint :: PresetColor c => c -> Percent -> c
  tint c w = interpolate (pctClamp w) (c, black)

-- | Interpolate two colors in the @CIE L*Ch* color space with a
-- weight.
--
-- Weight is applied left to right, so if a weight of 25% is supplied,
-- then the color on the left will be multiplied by 25% and the second
-- color will be multiplied by 75%.
instance Blendable CIELCH where
  interpolate w ((CIELCH al ac ah), (CIELCH bl bc bh)) =
    let w' = pct $ pctClamp w
        (CIELCH l c h) = (CIELCH (al - bl) (ac - bc) (ah - bh))
        (CIELCH nl nc nh) = omap (*w') (CIELCH l c (shortestPath h))
    in CIELCH (nl + al) (nc + ac) (nh + ah)

------------------------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------------------------
-- | Give the shortest path to the hue value.
shortestPath :: Double -> Double
shortestPath h | h > 180    = h - 360
               | h < (-180) = h + 360
               | otherwise  = h

-- | Give the decimal value for the given "percent" value.
--
-- The @Percent@ value may range from -100 to 100.
pct :: Percent -> Double
pct i = fromIntegral m / 100
  where
    m = max (-100) $ min 100 i

-- | Clamp a @Percent@ value in the range -100 to 100.
pctClamp :: Percent -> Percent
pctClamp i = max (min i 100) (-100)
