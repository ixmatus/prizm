Welcome!
=====

Prizm is a haskell library for dealing with colors. Please contribute!

My inspiration for writing this was Sass and Bourbon, both implement interesting color handling functions for
use in stylesheets and I wanted the same thing for use in [Clay](http://fvisser.nl/clay/) (also for
[Bentonite](https://github.com/ixmatus/bentonite)).

## Colour Module

There is some overlap with the Haskell Colour Module that already exists; however, this library is filling a couple of
needs the other doesn't satisfy.

## TODO

This module is getting kind of messy, I want to refactor how everything is organized. I also need to fix the types.

## Supported Algorithms

- sRGB <-> CIE XYZ
- CIE XYZ <-> CIE L\*a\*b\*
- CIE L\*\a\*b <-> CIE L\*C\*h

## Roadmap

### 0.3.0.0

I really want to clean up the API - right now stuff is just "everywhere" and the different color spaces should have
their own modules with their own convenience conversions to/from RGB to minimize the amount of function chaining
required.

I also want to figure out a way of testing the lighten/darken, interpolation, and hue transformation functions.

### 0.2.1.0

Blending is now switched over to using CIE L*Ch color space for more accurate blending.

### 0.2.0.0
Functions for

- color blending
- tint / shade
- darken / lighten

### 0.1.0.4
Conversion functions for sRGB to HEX.

### 0.1.0.3
All color types should have instances for Functor so fmap can work over their values.
All color types should have instances for Applicative to make computations cleaner.

### 0.1.0.2
Make sure all of the conversion formulas convert losslessly at an acceptable level of precision.

[General Color Formulas, Data, and Algorithms](http://www.brucelindbloom.com/index.html?Info.html)
[CIE Conversion Mathematics](http://rip94550.wordpress.com/2011/07/04/color-cielab-and-tristimulus-xyz/)
[Conversion Algorithm Sources](http://www.easyrgb.com/index.php?X=MATH&H=01)
[Good list of useful color manipulation](https://github.com/mikeemoo/ColorJizz-PHP/blob/master/src/MischiefCollective/ColorJizz/ColorJizz.php)
