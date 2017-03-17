{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import qualified HUnit.Blending as Blending
import qualified QC.CIE         as CIE
import qualified QC.RGB         as RGB
import           Test.Framework (Test, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "RGB" RGB.tests
  , testGroup "CIE" CIE.tests
  , testGroup "Blending" Blending.tests
  ]
