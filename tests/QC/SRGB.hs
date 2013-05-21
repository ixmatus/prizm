{-# LANGUAGE FlexibleInstances #-}

module QC.SRGB (tests) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Numeric

import Control.Monad

import Data.Prizm.Color.SRGB as S
import Data.Prizm.Color.CIE as C
import Data.Prizm.Types

instance Arbitrary (RGB Integer) where
    arbitrary = liftM3 RGB (choose (0, 255)) (choose (0, 255)) (choose (0, 255))

rgb2XYZ v = C.toRGB(S.toXYZ v) == v
rgb2HEX v = S.fromHex(S.toHex v) == v

tests = [
    testProperty "SRGB <-> CIE XYZ" rgb2XYZ,
    testProperty "HEX <-> SRGB" rgb2HEX
      ]
