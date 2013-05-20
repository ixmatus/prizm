Welcome!
=====

Prizm is a haskell library for dealing with colors. Please contribute!

My inspiration for writing this was Sass and Bourbon, both implement interesting color handling functions for
use in stylesheets and I wanted the same thing for use in [Clay](http://fvisser.nl/clay/) (also for
[Bentonite](https://github.com/ixmatus/bentonite)).

## Colour Module

There is some overlap with the Haskell Colour Module that already exists; however, this library is filling a couple of
needs the other doesn't satisfy. I also want to have a cleaner API to work with.

## Supported Algorithms

- sRGB -> CIE XYZ
- CIE XYZ -> sRGB

## Roadmap

### 0.1.0.1
Make sure all of the conversion formulas convert losslessly at an acceptable level of precision.

### 1.0.0.0
Implementation of conversions for all the CIE color representations and a converter for HEX to SRGB (trivial).

### 1.1.0.0
Implementations for

- color mixing
- tint / shade
- darken / lighten
- hue
- saturation
- inversion
[General Color Formulas, Data, and Algorithms](http://www.brucelindbloom.com/index.html?Info.html)
[CIE Conversion Mathematics](http://rip94550.wordpress.com/2011/07/04/color-cielab-and-tristimulus-xyz/)
[Conversion Algorithm Sources](http://www.easyrgb.com/index.php?X=MATH&H=01)
