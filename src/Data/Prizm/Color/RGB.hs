{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.RGB
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
--
-- Transformation functions and convenience functions to do the base
-- conversion between 'RGB' and 'CIEXYZ'.
----------------------------------------------------------------------------
module Data.Prizm.Color.RGB
( module Data.Prizm.Color.RGB.Types
) where

import           Control.Applicative
import           Data.Convertible.Base
import           Data.Monoid
import           Data.Prizm.Color.CIE.Matrices.RGB
import           Data.Prizm.Color.CIE.Types        as CIE
import           Data.Prizm.Color.RGB.Types
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types
import           Data.String
import qualified Data.Text                         as T
import           Data.Text.Read                    as R
import           Numeric                           (showHex)

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------
-- | Transform an RGB integer to be computed against a matrix.
transform :: Integer -> Double
transform v | dv > 0.04045 = (((dv + 0.055) / ap) ** 2.4) * 100
            | otherwise    = (dv / 12.92) * 100
  where dv = fromIntegral v / 255
        ap = 1.0 + 0.055

-- All credit for the below three functions go to the HSColour module.

-- | Show a colour in hexadecimal form, e.g. @#00aaff@
showRGB :: RGB -> Hex
showRGB c =
  (("#"++) . showHex2 r' . showHex2 g' . showHex2 b') ""
 where
  (unRGB -> ColorCoord(r',g',b')) = c
  showHex2 x | x <= 0xf = ("0"++) . showHex x
             | otherwise = showHex x

-- | Parse a 'Hex' into an s'RGB' type.
parse :: T.Text -> RGB
parse t =
  case T.uncons t of
    Just ('#', cs) | T.all isHex cs ->
      case T.unpack cs of
        [a, b, c, d, e, f, _g, _h] -> mkRGB (hex a b) (hex c d) (hex e f)
        [a, b, c, d, e, f      ]   -> mkRGB (hex a b) (hex c d) (hex e f)
        [a, b, c, _d            ]  -> mkRGB (hex a a) (hex b b) (hex c c)
        [a, b, c               ]   -> mkRGB (hex a a) (hex b b) (hex c c)
        _                          -> err
    _                              -> err

  where
    hex a b = either err fst (R.hexadecimal (T.singleton a <> T.singleton b))
    isHex a = (a >= 'a' && a <= 'f') || (a >= 'A' && a <= 'F') || (a >= '0' && a <= '9')
    err     = error "Invalid color string"

------------------------------------------------------------------------------
-- Convertible
------------------------------------------------------------------------------
instance Convertible RGB CIE.XYZ where
  -- | Convert an S'RGB' value to a 'CIE.XYZ' value with the default
  -- @d65@ illuminant matrix.
  safeConvert = Right . (toXYZMatrix d65SRGB)

instance Convertible RGB Hex where
  -- | Convert an S'RGB' value to a hexadecimal representation.
  safeConvert = Right . showRGB

instance Convertible Hex RGB where
  -- | Convert a hexadecimal value to an S'RGB'.
  safeConvert = Right . parse . fromString

-- | Convert an s'RGB' value to a 'CIE.XYZ' given a pre-calculated
-- illuminant matrix.
toXYZMatrix :: RGBtoXYZ -> RGB -> CIE.XYZ
toXYZMatrix (Matrix m) (unRGB -> ColorCoord(r,g,b)) =
  let t = ZipList ((transform . fromIntegral) <$> (clamp <$> [r,g,b]))
      [x,y,z] = (roundN 3) <$> ((zipTransform t) <$> m)
  in CIE.mkXYZ x y z
