{-# LANGUAGE OverloadedStrings #-}

module Data.Prizm.Color.Transform where

import Control.Applicative

roundN :: Integer -> Double -> Double
roundN n num = (fromInteger $ round $ num * (10^n)) / (10.0^^n)

truncateN :: Integer -> Double -> Double
truncateN n num = (fromInteger $ truncate $ num * (10^n)) / (10.0^^n)

zipTransform :: ZipList Double -> [Double] -> Double
zipTransform tv m = sum $ getZipList $ (*) <$> ZipList m <*> tv
