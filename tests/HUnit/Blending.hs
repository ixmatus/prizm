{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HUnit.Blending (tests) where

import           Data.Convertible
import           Data.Prizm.Color
import           Data.Prizm.Color.CIE           as CIE
import           Test.Framework                 (Test)
import           Test.Framework.Providers.HUnit as HUnit
import           Test.HUnit                     (Assertion, (@?=))

tests :: [Test]
tests =
  [ testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 0%"  $ blendPinkGreen 0  (mkRGB 255 0 255)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 10%" $ blendPinkGreen 10 (mkRGB 255 0 210)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 20%" $ blendPinkGreen 20 (mkRGB 255 0 163)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 30%" $ blendPinkGreen 30 (mkRGB 255 0 115)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 40%" $ blendPinkGreen 40 (mkRGB 255 51 67)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 50%" $ blendPinkGreen 50 (mkRGB 255 111 0)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 60%" $ blendPinkGreen 60 (mkRGB 255 152 0)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 70%" $ blendPinkGreen 70 (mkRGB 255 186 0)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 80%" $ blendPinkGreen 80 (mkRGB 222 213 0)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 90%" $ blendPinkGreen 90 (mkRGB 172 236 0)
  , testCase "Blend #ff00ff (pink) with #66ff00 (green) @ 100%" $ blendPinkGreen 100 (mkRGB 102 255 0)
  ]

blendPinkGreen :: Percent -> RGB -> Assertion
blendPinkGreen pct expected =
  let pink    :: CIE.LCH = convert $ mkRGB 255 0 255
      green   :: CIE.LCH = convert $ mkRGB 102 255 0
      blended :: RGB     = convert $ interpolate pct (pink,green)
  in blended @?= expected
