module QC.CIE (tests) where

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Control.Monad

import Data.Prizm.Color.CIE as C
import Data.Prizm.Color.Transform
import Data.Prizm.Types

instance Arbitrary CIE where
    arbitrary = oneof [(liftM3 XYZ (choose (0, 95.047)) (choose (0, 100.000)) (choose (0, 108.883))),
                       (liftM3 LAB (choose (0, 100)) (choose ((-129), 129)) (choose ((-129), 129)))]

tN = truncateN 11
rN = roundN 11

-- | This really needs applicatives here - I need to redefine the
-- types so that I can fmap functions over the color values.
-- 
-- This QuickCheck test guarantees a lossless conversion between CIE
-- XYZ and CIE L*a*b* at a precision of 11 decimal places.
xyz2LAB :: CIE -> Bool
xyz2LAB (XYZ x y z) =
    let nx = tN x
        ny = tN y
        nz = tN z
        nv = (XYZ nx ny nz)
        (XYZ x1 y1 z1) = C.toXYZ(C.toLAB(nv))
    in (XYZ (rN x1) (rN y1) (rN z1)) == nv
xyz2LAB (LAB l a b) =
    let nl = tN l
        na = tN a
        nb = tN b
        nv = (LAB nl na nb)
        (LAB l1 a1 b1) = C.toLAB(C.toXYZ nv)
    in (LAB (rN l1) (rN a1) (rN b1)) == nv

conversion :: CIE -> CIE
conversion v = C.toLAB(C.toXYZ v)

tests = [
    testProperty "CIE XYZ <-> CIE L*a*b*" xyz2LAB
      ]
