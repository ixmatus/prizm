{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.RGB.Types
-- Copyright   :  (C) 2017 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Color.RGB.Types where

import           Data.MonoTraversable
import           Data.Word

-- | A color in the @sRGB@ color space.
data RGB = RGB !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show)

-- | Monomorphic functor instances for the color spaces.
type instance Element RGB = Word8

instance MonoFunctor RGB where
  omap f (RGB r g b) = RGB (f r) (f g) (f b)
