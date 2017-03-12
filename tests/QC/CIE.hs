{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QC.CIE (tests) where

import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Control.Applicative
import           Control.Monad

import           Data.Convertible
import           Data.Prizm.Color.CIE
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types

instance Arbitrary CIEXYZ where
  arbitrary = do
    x <- choose (0, 95.047)
    y <- choose (0, 100.000)
    z <- choose (0, 108.883)

    return (CIEXYZ $ (rN <$> CIEXYZp x y z))

instance Arbitrary CIELAB where
  arbitrary = do
    l <- choose (0, 100)
    a <- choose ((-129), 129)
    b <- choose ((-129), 129)
    return (CIELAB $ (rN <$> CIELABp l a b))

rN :: Double -> Double
rN = roundN 11

xyz2LAB :: CIEXYZ -> Bool
xyz2LAB genVal = genVal == xyz
  where
    (CIEXYZ
     (CIEXYZ . (fmap rN) -> xyz)) =
      convert ((convert genVal) :: CIELAB)

lab2XYZ :: CIELAB -> Bool
lab2XYZ genVal = genVal == lab
  where
    (CIELAB
     (CIELAB . (fmap rN) -> lab)) =
      convert ((convert genVal) :: CIEXYZ)

lab2LCH :: CIELAB -> Bool
lab2LCH genVal = genVal == lch
  where
    (CIELAB
     (CIELAB . (fmap rN) -> lch)) =
      convert ((convert genVal) :: CIELCH)

tests :: [Test]
tests =
  [ testProperty "CIE XYZ  <-> CIE L*a*b*" xyz2LAB
  , testProperty "CIE L*ab <-> CIE XYZ"    lab2XYZ
  , testProperty "CIE L*ab <-> CIE L*Ch"   lab2LCH
  ]


