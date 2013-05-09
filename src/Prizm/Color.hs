-- module Prizm.Color where

-- pct :: Integer -> Double
-- pct = (/100) . fromIntegral . (max 0) . (min 100)

-- reduceByPct :: Integer -> Integer -> Integer
-- reduceByPct v pc = clamp $ floor (fromIntegral v - (fromIntegral v * (pct pc)))

-- increaseByPct :: Integer -> Integer -> Integer
-- increaseByPct v pc = clamp $ floor (fromIntegral v + (fromIntegral v * (pct pc)))

-- -- | @darken@ darkens a given color by a percentage.
-- -- 
-- -- The darken algorithm needs to darken all three colors if they are the same
-- darken :: Color -> Integer -> Color
-- darken (Rgba r g b a) i = Rgba (reduceByPct (fromIntegral r) i) (reduceByPct (fromIntegral g) i) (reduceByPct (fromIntegral b) i) a
-- darken (Hsla r g b a) i = Hsla (reduceByPct (fromIntegral r) i) (reduceByPct (fromIntegral g) i) (reduceByPct (fromIntegral b) i) a
-- darken (Other o)      _ = Other o

-- -- | @lighten@ lightens a given color by a percentage.
-- lighten :: Color -> Integer -> Color
-- lighten (Rgba r g b a) i = Rgba (increaseByPct (fromIntegral r) i) (increaseByPct (fromIntegral g) i) (increaseByPct (fromIntegral b) i) a
-- lighten (Hsla r g b a) i = Hsla (increaseByPct (fromIntegral r) i) (increaseByPct (fromIntegral g) i) (increaseByPct (fromIntegral b) i) a
-- lighten (Other o)      _ = Other o

-- -- (<.) shade
-- -- (.>) tint
