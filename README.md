Welcome!
========

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

# Interpolation Tests

  <div style="margin: auto; width: 500px;">
    <table>
      <tr>
        <td style="background-color: rgb(255,0,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(0,0,255); width: 80px; height: 80px;"></td>
      </tr>
      <tr>
        <td style="background-color: rgb(255,0,255); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(102,255,0); width: 80px; height: 80px;"></td>
      </tr>
    </table>
    <br />
    <br />
    <table>
      <tr>
        <td style="background-color: rgb(255,0,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,35); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,58); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,80); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,103); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(250,0,128); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(235,0,155); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(212,0,181); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(179,0,208); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(131,0,233); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(0,0,255); width: 80px; height: 80px;"></td>
      </tr>
    </table>
    <hr />
    <table>
      <tr>
        <td style="background-color: rgb(0,0,255); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(131,0,233); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(179,0,208); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(212,0,181); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(235,0,155); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(250,0,128); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,103); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,80); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,58); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,35); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,0); width: 80px; height: 80px;"></td>
      </tr>
    </table>
    <br />
    <br />
    <br />
    <br />
    <br />
    <br />
    <table>
      <tr>
        <td style="background-color: rgb(255,0,255); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,210); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,163); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,115); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,51,67); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,111,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,152,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,186,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(222,213,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(172,236,0); width: 80px; height: 80px;"></td>

        <td style="background-color: rgb(102,255,0); width: 80px; height: 80px;"></td>
      </tr>
    </table>
    <hr />
    <table>
      <tr>
        <td style="background-color: rgb(102,255,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(172,236,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(222,213,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,186,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,152,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,111,0); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,51,67); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,115); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,163); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,210); width: 80px; height: 80px;"></td>
        <td style="background-color: rgb(255,0,255); width: 80px; height: 80px;"></td>
      </tr>
    </table>
  </div>
