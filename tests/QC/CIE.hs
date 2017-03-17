{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QC.CIE (tests) where

import           Control.Monad                        (liftM3)
import           Data.Convertible
import           Data.MonoTraversable
import           Data.Prizm.Color
import           Data.Prizm.Color.CIE                 as CIE
import           Data.Prizm.Color.Transform
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 as QuickCheck
import           Test.QuickCheck

instance Arbitrary CIE.XYZ where
  arbitrary = liftM3 CIE.XYZ (choose (0, 95.047)) (choose (0, 100.000)) (choose (0, 108.883))

instance Arbitrary CIE.LAB where
  arbitrary = liftM3 CIE.LAB (choose (0, 100)) (choose ((-129), 129)) (choose ((-129), 129))

rN :: Double -> Double
rN = roundN 11

xyz2LAB :: CIE.XYZ -> Bool
xyz2LAB ((omap rN ) -> genVal) = genVal == xyz
  where
    ((omap rN) -> xyz) =
      convert ((convert genVal) :: CIE.LAB)

lab2XYZ :: CIE.LAB -> Bool
lab2XYZ ((omap rN ) -> genVal) = genVal == lab
  where
    ((omap rN) -> lab) =
      convert ((convert genVal) :: CIE.XYZ)

lab2LCH :: CIE.LAB -> Bool
lab2LCH ((omap rN ) -> genVal) = genVal == lch
  where
    ((omap rN) -> lch) =
      convert ((convert genVal) :: CIE.LCH)

tests :: [Test]
tests =
  [ QuickCheck.testProperty "CIE XYZ  <-> CIE L*a*b*" xyz2LAB
  , QuickCheck.testProperty "CIE L*ab <-> CIE XYZ   " lab2XYZ
  , QuickCheck.testProperty "CIE L*ab <-> CIE L*Ch  " lab2LCH
  ]


