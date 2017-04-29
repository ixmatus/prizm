{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.Transform
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Color.Transform where

import           Control.Applicative

-- | Round a number to the Nth decimal place.
roundN :: Integer -- ^ Nth place to round a number to
       -> Double  -- ^ Number to round
       -> Double
roundN n num = (fromInteger $ round $ num * (10^n)) / (10.0^^n)

-- | Transform a triplet of values from a working space matrix,
-- sequentially multiplying each value against a 'ZipList' of input
-- transformation values and taking the sum.
--
-- I'm sure there's a math-y word for what this is doing (affine
-- transformation?).
--
-- TODO: use a triple instead of a list! It is possible to goof up by
-- providing a list of elements greater than three.
zipTransform :: ZipList Double -- ^ ZipList of values to transform matrix triplet
             -> [Double]       -- ^ Working space triplet
             -> Double         -- ^ Sum of the sequentially applied transformation
zipTransform tv matrix = sum $ getZipList $ (*) <$> ZipList matrix <*> tv
