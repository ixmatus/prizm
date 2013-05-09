{-# LANGUAGE OverloadedStrings #-}

module Prizm.Color.Transform
(
  toXYZ
, fromXYZ
) where

import Prizm.Types

import Control.Applicative

rgbToXYZmatrix :: [[Double]]
rgbToXYZmatrix = [
  [0.4124, 0.3576, 0.1805],
  [0.2126, 0.7152, 0.0722],
  [0.0193, 0.1192, 0.9505]]

xyzToRGBmatrix :: [[Double]]
xyzToRGBmatrix = [
  [3.2406, (-1.5372), (-0.4986)],
  [(-0.9689), 1.8758, 0.0415],
  [0.0557, (-0.2040), 1.0570]]

-- | @rgbTransform@ transform an RGB integer to be computed against
-- the rgbToXYZ matrix.
rgbTransform :: Integer -> Double
rgbTransform v | dv > 0.04045 = (((dv + 0.055) / ap) ** 2.4) * 100
               | otherwise    = (dv / 12.92) * 100
    where dv = fromIntegral v / 255
          ap = 1.0 + 0.055

-- | @xyzTransform@ transform an XYZ integer to be computed against
-- the xyzToRGB matrix.
xyzTransform :: Double -> Integer
xyzTransform v | v > 0.0031308 = min (truncate ((1.055 * (v ** (1 / 2.4)) - 0.055) * 255)) 255
               | otherwise     = min (truncate ((12.92 * v) * 255)) 255

zipTransform :: ZipList Double -> [Double] -> Double
zipTransform tv m = sum $ getZipList $ (*) <$> ZipList m <*> tv

-- | @toXYZ@ convert an sRGB value to an CIE XYZ value.
-- 
-- sRGB is the standard CSS representation of RGB colors and we want
-- to be able to convert it to XYZ.
-- 
-- The Alpha channel value gets thrown away here.
toXYZ :: SRGB -> CIE
toXYZ (SRGB r g b) =
    let t = ZipList (rgbTransform <$> [r,g,b])
        [x,y,z] = (zipTransform t) <$> rgbToXYZmatrix
    in XYZ x y z

fromXYZ :: CIE -> Maybe SRGB
fromXYZ (LAB _ _ _) = Nothing
fromXYZ (XYZ x y z) =
    let t = ZipList ((/100) <$> [x,y,z])
        [r,g,b] = (xyzTransform) <$> ((zipTransform t) <$> xyzToRGBmatrix)
    in Just (SRGB r g b)
