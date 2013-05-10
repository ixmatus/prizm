{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import qualified QC.SRGB as SRGB

import Test.Framework (defaultMain, testGroup)

main = defaultMain tests

tests = [
     testGroup "srgb" SRGB.tests
      ]
