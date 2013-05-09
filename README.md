Welcome!
=====

Prizm is a haskell library for dealing with colors. Please contribute!

My inspiration for writing this was Sass and Bourbon, both implement interesting color handling functions for
use in stylesheets and I wanted the same thing for use in [Clay](http://fvisser.nl/clay/) (also for
[Bentonite](https://github.com/ixmatus/bentonite)).

## Supported Algorithms

- sRGB -> CIE XYZ
- CIE XYZ -> sRGB

## Roadmap

### 1.0.0
Implementation of conversions for all the CIE color representations and a converter for HEX to SRGB (trivial).

### 1.1.0
Implementations for

- color mixing
- tint / shade
- darken / lighten
- hue
- saturation
- inversion

[Algorithm Sources](http://www.easyrgb.com/index.php?X=MATH&H=01)
