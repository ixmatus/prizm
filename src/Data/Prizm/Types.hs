module Data.Prizm.Types where

import           Control.Applicative

-- | Working space matrix to convert from sRGB to CIE XYZ
newtype RGBtoXYZ = RGBtoXYZ [[Double]] deriving (Eq, Ord, Show)

-- | Working space matrix to convert from CIE XYZ to sRGB
newtype XYZtoRGB = XYZtoRGB [[Double]] deriving (Eq, Ord, Show)

-- | Hex format color code, eg '#AB9D92'
type Hex = String

type Percent = Integer

data RGB a = RGB !a !a !a
    deriving (Eq, Ord, Show)

data CIEXYZ a = CIEXYZ !a !a !a
    deriving (Eq, Ord, Show)

data CIELAB a = CIELAB !a !a !a
    deriving (Eq, Ord, Show)

data CIELCH a = CIELCH !a !a !a
    deriving (Eq, Ord, Show)

-- | Functor instances

instance Functor RGB where
    fmap f (RGB r g b) = (RGB (f r) (f g) (f b))

instance Functor CIEXYZ where
    fmap f (CIEXYZ x y z) = (CIEXYZ (f x) (f y) (f z))

instance Functor CIELAB where
    fmap f (CIELAB l a b) = (CIELAB (f l) (f a) (f b))

instance Functor CIELCH where
    fmap f (CIELCH l c h) = (CIELCH (f l) (f c) (f h))

-- | Applicative instances

instance Applicative RGB where
    pure t = RGB t t t
    (RGB f1 f2 f3) <*> (RGB r g b) = (RGB (f1 r) (f2 g) (f3 b))

instance Applicative CIEXYZ where
    pure t = CIEXYZ t t t
    CIEXYZ f1 f2 f3 <*> CIEXYZ x y z = CIEXYZ (f1 x) (f2 y) (f3 z)

instance Applicative CIELAB where
    pure t = CIELAB t t t
    CIELAB f1 f2 f3 <*> CIELAB l a b = CIELAB (f1 l) (f2 a) (f3 b)

instance Applicative CIELCH where
    pure t = CIELCH t t t
    CIELCH f1 f2 f3 <*> CIELCH l c h = CIELCH (f1 l) (f2 c) (f3 h)
