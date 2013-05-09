{-# LANGUAGE OverloadedStrings #-}

module Prizm.Color.Transform where

import Control.Applicative

zipTransform :: ZipList Double -> [Double] -> Double
zipTransform tv m = sum $ getZipList $ (*) <$> ZipList m <*> tv
