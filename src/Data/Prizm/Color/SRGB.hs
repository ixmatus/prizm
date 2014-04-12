-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.SRGB
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
--
-- Transformation functions and convenience functions to do the base
-- conversion between S'RGB' and 'CIEXYZ'.
----------------------------------------------------------------------------
module Data.Prizm.Color.SRGB
(
-- * Convert to CIEXYZ
  toXYZ
, toXYZMatrix

-- * Convert to Hex
, toHex
, fromHex
, clamp
) where

import           Numeric                       (showHex)

import           Data.Monoid
import           Data.Prizm.Color.Matrices.RGB
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types

import           Data.String
import qualified Data.Text                     as T
import           Data.Text.Read                as R

import           Control.Applicative

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- | Transform an RGB integer to be computed against
-- a matrix.
transform :: Integer -> Double
transform v | dv > 0.04045 = (((dv + 0.055) / ap) ** 2.4) * 100
            | otherwise    = (dv / 12.92) * 100
    where dv = fromIntegral v / 255
          ap = 1.0 + 0.055

-- | Clamp an integer with an upper-bound of 255 (the maximum RGB
-- value).
clamp :: Integer -> Integer
clamp i = max (min i 255) 0

-- All credit for the below three functions go to the HSColour module.

-- | Show a colour in hexadecimal form, e.g. \"#00aaff\"
showRGB :: RGB Integer -> Hex
showRGB c =
  (("#"++) . showHex2 r' . showHex2 g' . showHex2 b') ""
 where
  RGB r' g' b' = c
  showHex2 x | x <= 0xf = ("0"++) . showHex x
             | otherwise = showHex x

-- | Parse a 'Hex' into an 'RGB' type.
parse :: T.Text -> RGB Integer
parse t =
  case T.uncons t of
    Just ('#', cs) | T.all isHex cs ->
      case T.unpack cs of
        [a, b, c, d, e, f, _g, _h] -> RGB (hex a b) (hex c d) (hex e f)
        [a, b, c, d, e, f      ]   -> RGB (hex a b) (hex c d) (hex e f)
        [a, b, c, _d            ]  -> RGB (hex a a) (hex b b) (hex c c)
        [a, b, c               ]   -> RGB (hex a a) (hex b b) (hex c c)
        _                          -> err
    _                              -> err

  where
    hex a b = either err fst (R.hexadecimal (T.singleton a <> T.singleton b))
    isHex a = (a >= 'a' && a <= 'f') || (a >= 'A' && a <= 'F') || (a >= '0' && a <= '9')
    err     = error "Invalid color string"

------------------------------------------------------------------------------
-- Convert to XYZ
------------------------------------------------------------------------------

-- | Convert an S'RGB' value to a 'CIEXYZ' value.
toXYZ :: RGB Integer -> CIEXYZ Double
toXYZ = (toXYZMatrix d65SRGB)

-- | Convert an S'RGB' value to a 'CIEXYZ' given a pre-calculated
-- illuminant matrix.
--
-- It is recommended to use 'toXYZ' as it uses the most common
-- illuminant matrix.
toXYZMatrix :: RGBtoXYZ -> RGB Integer -> CIEXYZ Double
toXYZMatrix (RGBtoXYZ m) (RGB r g b) =
    let t = ZipList (transform <$> (clamp <$> [r,g,b]))
        [x,y,z] = (roundN 3) <$> ((zipTransform t) <$> m)
    in CIEXYZ x y z

------------------------------------------------------------------------------
-- Convert to Hex
------------------------------------------------------------------------------

-- | Convert an S'RGB' value to 'Hex'.
toHex :: RGB Integer -> Hex
toHex = showRGB

-- | Convert a 'Hex' to an S'RGB'.
fromHex :: Hex -> RGB Integer
fromHex = parse . fromString





