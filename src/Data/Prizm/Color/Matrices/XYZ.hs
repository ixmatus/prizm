module Data.Prizm.Color.Matrices.XYZ where

import Data.Prizm.Types (XYZtoRGB)

d50SRGB :: XYZtoRGB
d50SRGB =  [
  [3.1338561],[(-1.6168667)],[(-0.4906146)],
  [(-0.9787684)],[1.9161415],[0.0334540],
  [0.0719453],[(-0.2289914)],[1.4052427]]

d65SRGB :: XYZtoRGB
d65SRGB =  [
  [3.2404542, (-1.5371385), (-0.4985314)],
  [(-0.9692660), 1.8760108, 0.0415560],
  [0.0556434, (-0.2040259), 1.0572252]]

d50Adobe :: XYZtoRGB
d50Adobe =  [
  [1.9624274],[(-0.6105343)],[(-0.3413404)],
  [(-0.9787684)],[1.9161415],[0.0334540],
  [0.0286869],[(-0.1406752)],[1.3487655]]

d65Adobe :: XYZtoRGB
d65Adobe =  [
  [2.0413690],[(-0.5649464)],[(-0.3446944)],
  [(-0.9692660)],[1.8760108],[0.0415560],
  [0.0134474],[(-0.1183897)],[1.0154096]]

d50CIERGB :: XYZtoRGB
d50CIERGB =  [
  [2.3638081],[(-0.8676030)],[(-0.4988161)],
  [(-0.5005940)],[1.3962369],[0.1047562],
  [0.0141712],[(-0.0306400)],[1.2323842]]

eCIERGB :: XYZtoRGB
eCIERGB =  [
  [2.3706743],[(-0.9000405)],[(-0.4706338)],
  [(-0.5138850)],[1.4253036],[0.0885814],
  [0.0052982],[(-0.0146949)],[1.0093968]]
