module QC.SRGB (tests) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad

import Data.Prizm.Color.SRGB as S
import Data.Prizm.Color.CIE as C
import Data.Prizm.Types

instance Arbitrary RGB where
    arbitrary = liftM3 RGB (choose (0, 255)) (choose (0, 255)) (choose (0, 255))

rgb2XYZ v = C.toRGB(S.toXYZ v) == v

tests = [
    testProperty "toXYZ" rgb2XYZ
      ]
