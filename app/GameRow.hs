module GameRow (RowElement (Block), drawRow, RgbColor, Orientation (Vertical, Horizontal)) where

import Brick (Widget, emptyWidget, modifyDefAttr, str, (<+>), (<=>))
import Graphics.Vty (Color (RGBColor), withBackColor, withForeColor)

type RgbColor = (Float, Float, Float)

-- Block: position length
data RowElement = Block Int Int

data Orientation = Vertical | Horizontal

toRGBColor :: RgbColor -> Color
toRGBColor (r, g, b) = RGBColor (floor r) (floor g) (floor b)

-- Applies a transparent color on top of a base color
overlay :: Float -> RgbColor -> RgbColor -> RgbColor
overlay alpha (r, g, b) (r', g', b') =
  ( r * (1 - alpha) + r' * alpha,
    g * (1 - alpha) + g' * alpha,
    b * (1 - alpha) + b' * alpha
  )

withColor :: RgbColor -> RgbColor -> Widget n -> Widget n
withColor back fore = modifyDefAttr (\a -> withBackColor (withForeColor a (toRGBColor fore)) (toRGBColor back))

drawRow :: Orientation -> Int -> RgbColor -> [RowElement] -> Widget ()
drawRow orientation maxLength color elements = foldr combine emptyWidget $ do
  i <- [0 .. (maxLength - 1)]
  let progress = fromIntegral i / fromIntegral maxLength
  let progress' =
        case orientation of
          Horizontal -> 1 - progress
          Vertical -> progress
  let background = overlay (progress' * 0.3 + 0.05) (0, 0, 0) color
  return (withColor background background (str " "))
  where
    combine = case orientation of
      Horizontal -> (<+>)
      Vertical -> (<=>)
