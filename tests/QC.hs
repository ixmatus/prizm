{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import qualified QC.SRGB as SRGB
import qualified QC.CIE as CIE
--import qualified QC.Color as Color

import Test.Framework (defaultMain, testGroup)

main = defaultMain tests

tests = [
      testGroup "srgb" SRGB.tests
    , testGroup "cie" CIE.tests
--    , testGroup "color" Color.tests
      ]
