{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QC.CIE (tests) where

import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Control.Applicative
import           Control.Monad

import           Data.Prizm.Color.CIE.LAB             as LB
import           Data.Prizm.Color.CIE.LCH             as LC
import           Data.Prizm.Color.CIE.XYZ             as X
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types

instance Arbitrary (CIEXYZ Double) where
    arbitrary = liftM3 CIEXYZ (choose (0, 95.047)) (choose (0, 100.000)) (choose (0, 108.883))

instance Arbitrary (CIELAB Double) where
    arbitrary = liftM3 CIELAB (choose (0, 100)) (choose ((-129), 129)) (choose ((-129), 129))

rN :: Double -> Double
rN = roundN 11

-- | This really needs applicatives here - I need to redefine the
-- types so that I can fmap functions over the color values.
--
-- This QuickCheck test guarantees a lossless conversion between CIE
-- XYZ and CIE L*a*b* at a precision of 11 decimal places.
xyz2LAB :: CIEXYZ Double -> Bool
xyz2LAB v =
    let nv = rN <$> v
    in (rN <$> LB.toXYZ(X.toLAB nv)) == nv

lab2XYZ :: CIELAB Double -> Bool
lab2XYZ v =
    let nv = rN <$> v
    in (rN <$> X.toLAB(LB.toXYZ nv)) == nv

lab2LCH :: CIELAB Double -> Bool
lab2LCH v =
    let nv = rN <$> v
    in (rN <$> LC.toLAB(LB.toLCH nv)) == nv

tests :: [Test]
tests = [
    testProperty "CIE XYZ <-> CIE L*a*b*" xyz2LAB,
    testProperty "CIE L*ab <-> CIE XYZ" lab2XYZ,
    testProperty "CIE L*ab <-> CIE L*Ch" lab2LCH
      ]
