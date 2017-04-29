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
import           Data.Bifunctor                    as Bifunctor
import           Data.Convertible.Base
import qualified Data.Foldable                     as Foldable
import           Data.Monoid
import           Data.Prizm.Color.CIE.Matrices.RGB
import           Data.Prizm.Color.CIE.Types        as CIE
import           Data.Prizm.Color.RGB.Types
import           Data.Prizm.Color.Transform
import           Data.Prizm.Types
import qualified Data.Text                         as Text
import qualified Data.Text.Read                    as Text.Read
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

-- | Encode a 256-cubed 'RGB' color into a 'HexRGB', e.g. @#00aaff@
encodeHex :: RGB -> HexRGB
encodeHex (RGB rgb) = HexRGB (Text.pack $ "#" <> (Foldable.foldMap encode rgb))
 where
  encode x | x <= 0xf  = "0"<>(showHex x "")
           | otherwise = showHex x ""

-- | Decode a 'HexRGB' encoded RGB color (e.g: #D60CD3) into 256-cubed
-- 'RGB'.
decodeHex :: HexRGB -> Either String RGB
decodeHex (HexRGB orig@(Text.uncons -> cell)) =
  case cell of
    Just ('#', rest) ->
      case Text.unpack rest of
        [a, b, c, d, e, f, _g, _h]
          -> mkRGB <$> hex a b <*> hex c d <*> hex e f

        [a, b, c, d, e, f]
          -> mkRGB <$> hex a b <*> hex c d <*> hex e f

        [a, b, c, _d]
          -> mkRGB <$> hex a a <*> hex b b <*> hex c c

        [a, b, c]
          -> mkRGB <$> hex a a <*> hex b b <*> hex c c
        _ -> can'tDecode
    _     -> can'tDecode

  where
    hex :: Char -> Char -> Either String Int
    hex a b = Bifunctor.second fst $ Text.Read.hexadecimal (Text.singleton a <> Text.singleton b)

    can'tDecode = Left $ "cannot decode "++(Text.unpack orig)

------------------------------------------------------------------------------
-- Convertible
------------------------------------------------------------------------------
instance Convertible RGB CIE.XYZ where
  -- | Convert an S'RGB' value to a 'CIE.XYZ' value with the default
  -- @d65@ illuminant matrix.
  safeConvert = Right . (toXYZMatrix d65SRGB)

instance Convertible RGB HexRGB where
  -- | Convert a 256-cubed 'RGB' color to a hexadecimal encoding.
  safeConvert = Right . encodeHex

instance Convertible HexRGB RGB where
  -- | Convert a hexadecimal value to an 'RGB'.
  safeConvert v = Bifunctor.first convertibleError $ decodeHex v
    where
      convertibleError msg =
        ConvertError
        { convSourceValue  = show v
        , convSourceType   = "HexRGB"
        , convDestType     = "RGB"
        , convErrorMessage = msg
        }

-- | Convert an s'RGB' value to a 'CIE.XYZ' given a pre-calculated
-- illuminant matrix.
toXYZMatrix :: RGBtoXYZ -> RGB -> CIE.XYZ
toXYZMatrix (Matrix m) (unRGB -> ColorCoord(r,g,b)) =
  let t = ZipList ((transform . fromIntegral) <$> (clamp <$> [r,g,b]))
      [x,y,z] = (roundN 3) <$> ((zipTransform t) <$> m)
  in CIE.mkXYZ x y z
