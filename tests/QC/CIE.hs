{-# LANGUAGE FlexibleInstances #-}

module QC.CIE (tests) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad
import Control.Applicative

import Data.Prizm.Color.CIE as C
import Data.Prizm.Color.Transform
import Data.Prizm.Types

instance Arbitrary (CIEXYZ Double) where
    arbitrary = liftM3 CIEXYZ (choose (0, 95.047)) (choose (0, 100.000)) (choose (0, 108.883))

instance Arbitrary (CIELAB Double) where
    arbitrary = liftM3 CIELAB (choose (0, 100)) (choose ((-129), 129)) (choose ((-129), 129))

tN = truncateN 11
rN = roundN 11

-- | This really needs applicatives here - I need to redefine the
-- types so that I can fmap functions over the color values.
-- 
-- This QuickCheck test guarantees a lossless conversion between CIE
-- XYZ and CIE L*a*b* at a precision of 11 decimal places.
xyz2LAB :: CIEXYZ Double -> Bool
xyz2LAB v =
    let nv = tN <$> v
    in (rN <$> C.toXYZ(C.toLAB nv)) == nv

lab2XYZ :: CIELAB Double -> Bool
lab2XYZ v =
    let nv = tN <$> v
    in (rN <$> C.toLAB(C.toXYZ nv)) == nv

tests = [
    testProperty "CIE XYZ <-> CIE L*a*b*" xyz2LAB,
    testProperty "CIE L*a*b* <-> CIE XYZ" lab2XYZ
      ]
