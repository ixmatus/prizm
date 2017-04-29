{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Convertible
import           Data.Prizm.Color
import           Data.Prizm.Color.CIE as CIE

main :: IO ()
main = do
  -- Convert RGB colors to the CIE.LCH color space
  let green :: CIE.LCH = convert $ mkRGB 102 255 0
      pink  :: CIE.LCH = convert $ mkRGB 255 0 255

      -- Blend with a weight of 50%
      blended50 = pink <~> green

      -- Blend with a weight of 20%
      blended20 = interpolate 20 (pink,green)

  -- Print the CIE.LCH representation
  putStrLn $ show blended50

  -- Print the RGB representation of the blended color
  putStrLn . show $ ((convert blended20) :: RGB)

  -- Print the CSS-friendly hexadecimal RGB representation of the blended color
  putStrLn . show $ ((convert blended20) :: Hex)
