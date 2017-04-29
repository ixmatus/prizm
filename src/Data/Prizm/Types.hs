{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Prizm.Types
-- Copyright   :  (C) 2013 Parnell Springmeyer
-- License     :  BSD3
-- Maintainer  :  Parnell Springmeyer <parnell@digitalmentat.com>
-- Stability   :  stable
-----------------------------------------------------------------------------
module Data.Prizm.Types
( HexRGB(..)
, Percent
, ColorCoord(..)
, (<$$$>)
, (<***>)
) where

import           Data.Monoid
import           Data.Text

-- | Map a function over each element of a triple..
(<$$$>) :: (a -> b) -> (a,a,a) -> (b,b,b)
(<$$$>) f (a1,a2,a3) = (f a1, f a2, f a3)

-- | Sequentially apply a triple to another triple, like Applicative's
-- sequential application but specialized to triples.
(<***>) :: ((a -> b), (a -> b), (a -> b)) -> (a,a,a) -> (b,b,b)
(<***>) (fa1, fa2, fa3) (b1,b2,b3) = (fa1 b1, fa2 b2, fa3 b3)

-- | A generic representation of a color triple; this may be *any*
-- color space so you should not construct colors with 'ColorCoord'
-- and instead you should use specialized color constructors from the
-- 'Data.Prizm.Color.RGB' module or 'Data.Prizm.Color.CIE' module.
newtype ColorCoord a = ColorCoord (a, a, a)
  deriving (Show, Eq, Ord, Read)

-- | Hexadecimal encoded RGB color with an octothorpe prefix; e.g:
-- @#AB9D92@.
--
-- Please see 'RGB.decodeHex'.
newtype HexRGB = HexRGB { unHexRGB :: Text }
  deriving (Show, Eq, Ord, Read)

-- | A percent value ranging from -100 to 100; e.g: -82%, 80%, 10%.
type Percent = Integer

instance Functor ColorCoord where
  fmap f (ColorCoord (a,b,c)) = ColorCoord (f a, f b, f c)

instance Applicative ColorCoord where
  pure c = ColorCoord (c,c,c)
  (ColorCoord (fa,fb,fc)) <*> (ColorCoord (a,b,c)) = ColorCoord (fa a, fb b, fc c)

instance Foldable ColorCoord where
  foldMap f (ColorCoord (a,b,c)) = f a <> f b <> f c

instance Traversable ColorCoord where
  traverse f (ColorCoord (a,b,c)) = ColorCoord <$> ((,,) <$> f a <*> f b <*> f c)

instance Monad ColorCoord where
  ColorCoord (a,b,c) >>= f = ColorCoord (a',b',c')
    where
      ColorCoord(a',_,_) = f a
      ColorCoord(_,b',_) = f b
      ColorCoord(_,_,c') = f c
