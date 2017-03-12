{-# LANGUAGE FlexibleInstances #-}

module QC.SRGB (tests) where

import           Control.Monad
import           Data.Convertible
import           Numeric
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Data.Prizm.Color.CIE
import           Data.Prizm.Color.SRGB                as S
import           Data.Prizm.Types

instance Arbitrary RGB where
  arbitrary = do
    r <- choose (0, 255)
    g <- choose (0, 255)
    b <- choose (0, 255)
    return (RGB $ RGBp r g b)

rgb2XYZ :: RGB -> Bool
rgb2XYZ gVal = gVal == iso
  where
    iso = convert ((convert gVal) :: CIEXYZ)

rgb2HEX :: RGB -> Bool
rgb2HEX gVal = gVal == iso
  where
    iso = convert ((convert gVal) :: Hex)

tests = [
    testProperty "SRGB <-> CIE XYZ" rgb2XYZ,
    testProperty "HEX <-> SRGB" rgb2HEX
      ]
