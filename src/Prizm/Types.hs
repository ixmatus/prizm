module Prizm.Types where

data SRGB
    = SRGB Integer Integer Integer
    deriving (Eq, Ord, Show)

data CIE
    = LAB Double Double Double
    | XYZ Double Double Double
    deriving (Eq, Ord, Show)
