module QC.CIE (tests) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad

import Data.Prizm.Color.CIE as C
import Data.Prizm.Types

instance Arbitrary CIE where
    arbitrary = oneof [(liftM3 XYZ (choose (0, 95.047)) (choose (0, 100.000)) (choose (0, 108.883))),
                       (liftM3 LAB (choose (0, 100)) (choose ((-129), 129)) (choose ((-129), 129)))]

xyz2LAB :: CIE -> Bool
xyz2LAB v = C.toLAB(C.toXYZ v) == v

tests = [
    testProperty "toLAB" xyz2LAB
      ]
