module Data.Prizm.Color.Matrices.RGB where

import Data.Prizm.Types (RGBtoXYZ)

d50SRGB :: RGBtoXYZ
d50SRGB =  [
  [0.4360747],[0.3850649],[0.1430804],
  [0.2225045],[0.7168786],[0.0606169],
  [0.0139322],[0.0971045],[0.7141733]]

d65SRGB :: RGBtoXYZ
d65SRGB =  [
  [0.4124564, 0.3575761, 0.1804375],
  [0.2126729, 0.7151522, 0.0721750],
  [0.0193339, 0.1191920, 0.9503041]]

d50Adobe :: RGBtoXYZ
d50Adobe =  [
  [0.6097559],[0.2052401],[0.1492240],
  [0.3111242],[0.6256560],[0.0632197],
  [0.0194811],[0.0608902],[0.7448387]]

d65Adobe :: RGBtoXYZ
d65Adobe =  [
  [0.5767309],[0.1855540],[0.1881852],
  [0.2973769],[0.6273491],[0.0752741],
  [0.0270343],[0.0706872],[0.9911085]]

d50CIE :: RGBtoXYZ
d50CIE =  [
  [0.4868870],[0.3062984],[0.1710347],
  [0.1746583],[0.8247541],[0.0005877],
  [(-0.0012563)],[0.0169832],[0.8094831]]

eCIE :: RGBtoXYZ
eCIE =  [
  [0.4887180],[0.3106803],[0.2006017],
  [0.1762044],[0.8129847],[0.0108109],
  [0.0000000],[0.0102048],[0.9897952]]
