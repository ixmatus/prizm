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

import           Data.MonoTraversable

-- | A color in the @CIE XYZ@ color space.
data XYZ = XYZ !Double !Double !Double
  deriving (Eq, Ord, Show)

-- | A color in the @CIE L*a*b*@ color space.
data LAB = LAB !Double !Double !Double
  deriving (Eq, Ord, Show)

-- | A color in the @CIE L*C*h(uv)@ color space.
data LCH = LCH !Double !Double !Double
  deriving (Eq, Ord, Show)

type instance Element XYZ = Double
type instance Element LCH = Double
type instance Element LAB = Double

instance MonoFunctor XYZ where
  omap f (XYZ x y z) = XYZ (f x) (f y) (f z)
instance MonoFunctor LAB where
  omap f (LAB l a b) = LAB (f l) (f a) (f b)
instance MonoFunctor LCH where
  omap f (LCH l c h) = LCH (f l) (f c) (f h)
