# Welcome!
![Hackage Version](https://img.shields.io/hackage/v/prizm.svg?style=flat)
![Travis CI Status](https://travis-ci.org/ixmatus/prizm.svg?branch=master)

`prizm` is a Haskell library for transforming colors. Specifically, providing
functions for transforming between different color spaces (`CIE` and `sRGB`),
interpolating colors and adjusting the tint, shade, hue, or lightness of a
color.

The inspiration for this library came from a desire to blend two colors
represented in the `sRGB` color space. My research about color blending and
color space representation in the computer led me to the conclusion that the
`CIE L*Ch` color space is the most effective for blending because it most
accurately represents how the human eye sees hue and therefore preserves (and
blends) hue the most accurately.

## Quickstart
```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Convertible
import           Data.Prizm.Color
import           Data.Prizm.Color.CIE as CIE

main :: IO ()
main = do
  -- Convert RGB colors to the CIE.LCH color space
  let green :: CIE.LCH = convert $ mkRGB 102 255 0
      pink  :: CIE.LCH = convert $ mkRGB 255 0 255

      -- Blend with a weight of 50%
      blended50 = pink <~> green

      -- Blend with a weight of 20%
      blended20 = interpolate 20 (pink,green)

  -- Print the CIE.LCH representation
  putStrLn $ show blended50

  -- Print the RGB representation of the blended color
  putStrLn . show $ ((convert blended20) :: RGB)

  -- Print the RGB color in a hexadecimal encoding
  putStrLn . show $ ((convert blended20) :: HexRGB)

      
```

## Supported Algorithms
- `sRGB     <-> CIE XYZ `
- `CIE XYZ  <-> CIE L*ab`
- `CIE L*ab <-> CIE L*Ch`

## Supported Functions
- Color interpolation
- Tinting / Darkening
- Lightness
- Hue
- Chroma/Saturation

## Examples
[Example blending with CIELCH converted back to RGB](./blending-test.html).

# References
- [General Color Formulas, Data, and Algorithms](http://www.brucelindbloom.com)
- [CIE Conversion Mathematics](http://rip94550.wordpress.com/2011/07/04/color-cielab-and-tristimulus-xyz/)
- [Conversion Algorithm Sources](http://www.easyrgb.com/index.php?X=MATH&H=01)
- [Good list of useful color manipulation formulas](https://github.com/mikeemoo/ColorJizz-PHP/blob/master/src/MischiefCollective/ColorJizz/ColorJizz.php)

