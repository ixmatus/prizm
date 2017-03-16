{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import qualified QC.CIE         as CIE
import qualified QC.SRGB        as SRGB
import           Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "SRGB" SRGB.tests
  , testGroup "CIE"  CIE.tests
  ]
