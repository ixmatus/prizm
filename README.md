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
- CIE XYZ <-> CIE L\*a\*b\*

## Roadmap

### 0.1.0.2
Make sure all of the conversion formulas convert losslessly at an acceptable level of precision.

### 0.1.0.3
All color types should have instances for Functor so fmap can work over their values.
All color types should have instances for Applicative to make computations cleaner.

### 0.1.0.4
Conversion functions for sRGB to HEX.

### 0.2.0.0
Functions for

- color blending
- tint / shade
- darken / lighten

[General Color Formulas, Data, and Algorithms](http://www.brucelindbloom.com/index.html?Info.html)
[CIE Conversion Mathematics](http://rip94550.wordpress.com/2011/07/04/color-cielab-and-tristimulus-xyz/)
[Conversion Algorithm Sources](http://www.easyrgb.com/index.php?X=MATH&H=01)
