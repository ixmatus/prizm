{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Types
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Types where

import           Data.Word

-- | Working space matrix to convert from sRGB to CIE XYZ.
newtype RGBtoXYZ = RGBtoXYZ [[Double]]
  deriving (Eq, Ord, Show)

-- | Working space matrix to convert from CIE XYZ to sRGB.
newtype XYZtoRGB = XYZtoRGB [[Double]]
  deriving (Eq, Ord, Show)

-- | Hex format color code, eg '#AB9D92'.
type Hex = String

type Percent = Integer

data RGBp a = RGBp !a !a !a
    deriving (Eq, Ord, Show)
newtype RGB = RGB (RGBp Word8)

data CIEXYZp a = CIEXYZp !a !a !a
    deriving (Eq, Ord, Show)
newtype CIEXYZ = CIEXYZ (CIEXYZp Double)

data CIELABp a = CIELABp !a !a !a
    deriving (Eq, Ord, Show)
newtype CIELAB = CIELAB (CIELABp Double)

data CIELCHp a = CIELCHp !a !a !a
    deriving (Eq, Ord, Show)
newtype CIELCH = CIELCH (CIELCHp Double)

-- | Functor instances
instance Functor RGBp where
    fmap f (RGBp r g b) = RGBp (f r) (f g) (f b)

instance Functor CIEXYZp where
    fmap f (CIEXYZp x y z) = CIEXYZp (f x) (f y) (f z)

instance Functor CIELABp where
    fmap f (CIELABp l a b) = CIELABp (f l) (f a) (f b)

instance Functor CIELCHp where
    fmap f (CIELCHp l c h) = CIELCHp (f l) (f c) (f h)

-- | Applicative instances
--
-- Not sure how intuitive these instances are...though.
instance Applicative RGBp where
    pure t = RGBp t t t
    (RGBp f1 f2 f3) <*> (RGBp r g b) = RGBp (f1 r) (f2 g) (f3 b)

instance Applicative CIEXYZp where
    pure t = CIEXYZp t t t
    CIEXYZp f1 f2 f3 <*> CIEXYZp x y z = CIEXYZp (f1 x) (f2 y) (f3 z)

instance Applicative CIELABp where
    pure t = CIELABp t t t
    CIELABp f1 f2 f3 <*> CIELABp l a b = CIELABp (f1 l) (f2 a) (f3 b)

instance Applicative CIELCHp where
    pure t = CIELCHp t t t
    CIELCHp f1 f2 f3 <*> CIELCHp l c h = CIELCHp (f1 l) (f2 c) (f3 h)

-- | Presets for a color.
class PresetColor c where
  white :: c
  black :: c

instance PresetColor CIELCH where
  white = CIELCH $ CIELCHp 0.0 0.0 360.0
  black = CIELCH $ CIELCHp 100.0 0.0 360.0

-- | A blendable color.
class Blendable c where
  interpolate :: Percent -> (c,c) -> c

  -- | Blend two @Blendable@ colors using an interpolation weight of
  -- 50%.
  (<~>) :: c -> c -> c
  (<~>) l r = interpolate 50 (l,r)

  -- | Shade a color by blending it using a weight and the color black.
  shade :: PresetColor c => c -> Percent -> c
  shade c w = interpolate (pctClamp w) (c, white)

  -- | Tint a color by blending it using a weight and the color white.
  tint :: PresetColor c => c -> Percent -> c
  tint c w = interpolate (pctClamp w) (c, black)

-- | Interpolate two colors with a weight.
--
-- Weight is applied left to right, so if a weight of 25% is supplied,
-- then the color on the left will be multiplied by 25% and the second
-- color will be multiplied by 75%.
instance Blendable CIELCH where
  interpolate w (CIELCH a, CIELCH b) =
    let w' = pct $ pctClamp w
        (CIELCHp l c h) = (-) <$> b <*> a
        a' = (*w') <$> (CIELCHp l c (shortestPath h))
    in CIELCH $ (+) <$> a' <*> a

------------------------------------------------------------------------------
-- Utility Functions
------------------------------------------------------------------------------
shortestPath :: Double -> Double
shortestPath h | h > 180    = h - 360
               | h < (-180) = h + 360
               | otherwise  = h

-- pct :: Integer -> Double
-- pct = (/100) . fromIntegral . (max (-100)) . (min 100)

pct :: Integer -> Double
pct i = fromIntegral m / 100
  where
    m = max (-100) $ min 100 i

pctClamp :: Integer -> Integer
pctClamp i = max (min i 100) 0
