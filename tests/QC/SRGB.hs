{-# LANGUAGE OverloadedStrings #-}

module QC.SRGB (tests) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Data.Prizm.Color.SRGB as S
import Data.Prizm.Color.CIE as C
import Data.Prizm.Types

instance Arbitrary SRGB where
    arbitrary = do
        r <- choose (0, 255)
        g <- choose (0, 255)
        b <- choose (0, 255)
        return (SRGB r g b)

rgb2XYZ v = C.toRGB(S.toXYZ v) == v

tests = [
    testProperty "toXYZ" rgb2XYZ
      ]
