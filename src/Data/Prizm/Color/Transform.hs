{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Color.Transform
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@ixmat.us>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Color.Transform where

import           Control.Applicative

-- | Round a number to the Nth place.
roundN :: Integer -> Double -> Double
roundN n num = (fromInteger $ round $ num * (10^n)) / (10.0^^n)

-- | Truncate a number to the Nth place.
truncateN :: Integer -> Double -> Double
truncateN = roundN

zipTransform :: ZipList Double -> [Double] -> Double
zipTransform tv m = sum $ getZipList $ (*) <$> ZipList m <*> tv
