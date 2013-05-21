-- {-# LANGUAGE FlexibleInstances #-}

-- module QC.Color (tests) where

-- import Test.Framework.Providers.QuickCheck2 (testProperty)
-- import Test.QuickCheck

-- import Control.Monad
-- import Control.Applicative

-- import Data.Prizm.Color.CIE as C
-- import Data.Prizm.Color
-- import Data.Prizm.Types

-- instance Arbitrary (CIEXYZ Double) where
--     arbitrary = liftM3 CIEXYZ (choose (0, 95.047)) (choose (0, 100.000)) (choose (0, 108.883))

-- dlColor :: CIEXYZ Double -> Percent -> Bool
-- dlColor c w = (lighten (darken c w) w) == c

-- tests = [
--     testProperty "darken/lighten color" dlColor
--       ]
