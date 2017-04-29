-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.CIE.Chroma.Illuminant
-- Copyright   :  (C) 2017 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- These values came from Bruce Lindbloom's website: <https://web.archive.org/web/20161110173539/http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html Chromatic Adaptation>
--
-- For future reference (also found in the above linked website), here
-- is a list of the reference white illuminant values:
--
-- *       X       Y       Z
-- * @A    1.09850 1.00000 0.35585@
-- * @B    0.99072 1.00000 0.85223@
-- * @C    0.98074 1.00000 1.18232@
-- * @D50  0.96422 1.00000 0.82521@
-- * @D55  0.95682 1.00000 0.92149@
-- * @D65  0.95047 1.00000 1.08883@
-- * @D75  0.94972 1.00000 1.22638@
-- * @E    1.00000 1.00000 1.00000@
-- * @F2   0.99186 1.00000 0.67393@
-- * @F7   0.95041 1.00000 1.08747@
-- * @F11  1.00962 1.00000 0.64350@
-----------------------------------------------------------------------------
module Data.Prizm.Color.CIE.Chroma.Illuminant
( RefWhite(..)
, a, b, c, d50
, d55, d65, d75
, e, f2, f7, f11
) where

-- | Reference white tristimulus value.
newtype RefWhite = Tristimulus (Double, Double, Double)
  deriving (Eq, Ord, Show)

a :: RefWhite
a = Tristimulus(190.850, 100.000, 35.585)

b :: RefWhite
b = Tristimulus(99.072, 100.000, 85.223)

c :: RefWhite
c = Tristimulus(98.074, 100.000, 118.232)

d50 :: RefWhite
d50 = Tristimulus(96.422, 100.000, 82.521)

d55 :: RefWhite
d55 = Tristimulus(95.682, 100.000, 92.149)

d65 :: RefWhite
d65 = Tristimulus(95.047, 100.000, 108.883)

d75 :: RefWhite
d75 = Tristimulus(94.972, 100.000, 122.638)

e :: RefWhite
e = Tristimulus(100.000, 100.000, 100.000)

f2 :: RefWhite
f2 = Tristimulus(99.186, 100.000, 67.393)

f7 :: RefWhite
f7 = Tristimulus(95.041, 100.000, 108.747)

f11 :: RefWhite
f11 = Tristimulus(100.962, 100.000, 64.350)
