Welcome!
=====

Prizm is a haskell library for dealing with colors. Please contribute!

My inspiration for writing this was Sass and Bourbon, both implement interesting color handling functions for
use in stylesheets and I wanted the same thing for use in [Clay](http://fvisser.nl/clay/) (also for
[Bentonite](https://github.com/ixmatus/bentonite)).

## Colour Module

There is some overlap with the Haskell Colour Module that already exists; however, this library is filling a couple of
needs the other doesn't satisfy.

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

All of these functions operate on color within the CIE L\*Ch representation. The percentage values may range between
-100 and 100.

[General Color Formulas, Data, and Algorithms](http://www.brucelindbloom.com/index.html?Info.html)

[CIE Conversion Mathematics](http://rip94550.wordpress.com/2011/07/04/color-cielab-and-tristimulus-xyz/)

[Conversion Algorithm Sources](http://www.easyrgb.com/index.php?X=MATH&H=01)

[Good list of useful color manipulation](https://github.com/mikeemoo/ColorJizz-PHP/blob/master/src/MischiefCollective/ColorJizz/ColorJizz.php)
