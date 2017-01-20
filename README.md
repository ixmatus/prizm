# Welcome!
![Hackage Version](https://img.shields.io/hackage/v/prizm.svg?style=flat)
![Travis CI Status](https://travis-ci.org/ixmatus/prizm.svg?branch=master)

`prizm` is a Haskell library for computing with colors. Specifically providing
functions for transforming between different color spaces, interpolating colors
and adjusting the tint, shade, hue, or lightness or a color.

My inspiration for writing this was Sass and Bourbon, both implement interesting
functions for computing with colors for use in stylesheets and I wanted the same
thing for use in [Clay](http://fvisser.nl/clay/).

## Colour Module
There is some overlap with the Haskell Colour Module that already exists;
however, this library is filling a couple of needs the other doesn't
satisfy. Primarily transformations and color mutations in the CIE L*Ch space.

## Supported Algorithms
- sRGB <-> CIE XYZ
- CIE XYZ <-> CIE L\*ab
- CIE L\*ab <-> CIE L\*Ch

## Supported Functions
- Color interpolation
- Tinting / Darkening
- Lightness
- Hue
- Chroma/Saturation

All of these functions operate on color within the CIE L\*Ch representation. The
percentage values may range between -100 and 100.

[General Color Formulas, Data, and Algorithms](http://www.brucelindbloom.com/index.html?Info.html)
[CIE Conversion Mathematics](http://rip94550.wordpress.com/2011/07/04/color-cielab-and-tristimulus-xyz/)
[Conversion Algorithm Sources](http://www.easyrgb.com/index.php?X=MATH&H=01)
[Good list of useful color manipulation formulas](https://github.com/mikeemoo/ColorJizz-PHP/blob/master/src/MischiefCollective/ColorJizz/ColorJizz.php)
