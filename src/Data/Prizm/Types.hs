module Data.Prizm.Types where

type RGBtoXYZ = [[Double]]
type XYZtoRGB = [[Double]]

data RGB
    = RGB Integer Integer Integer
    deriving (Eq, Ord, Show)

data CIE
    = XYZ Double Double Double
    | LAB Double Double Double
    deriving (Eq, Ord, Show)
