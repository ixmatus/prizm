{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QC.SRGB (tests) where

import           Control.Monad                        (liftM3)
import           Data.Convertible
import           Data.Prizm.Color
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 as QuickCheck
import           Test.QuickCheck

instance Arbitrary RGB where
  arbitrary = liftM3 RGB (choose rgbRange) (choose rgbRange) (choose rgbRange)
    where
      rgbRange = (0, 255)

rgb2XYZ :: RGB -> Bool
rgb2XYZ gVal = gVal == iso
  where
    iso = convert ((convert gVal) :: CIEXYZ)

rgb2HEX :: RGB -> Bool
rgb2HEX gVal = gVal == iso
  where
    iso = convert ((convert gVal) :: Hex)

tests :: [Test]
tests =
  [ QuickCheck.testProperty "SRGB <-> CIE XYZ" rgb2XYZ
  , QuickCheck.testProperty "HEX  <-> SRGB   " rgb2HEX
  ]
