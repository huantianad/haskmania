module GameRow (RowElement (Block), drawRow, RgbColor) where

import Brick (Widget, bg, emptyWidget, fg, modifyDefAttr, on, str)
import Brick.Widgets.Core ((<+>))
import Graphics.Vty (Color (RGBColor), withBackColor, withForeColor, withStyle)

-- Block: position length
data RowElement = Block Float Int

type RgbColor = (Float, Float, Float)

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

drawRow :: Int -> RgbColor -> [RowElement] -> Widget ()
drawRow width color elements = foldr (<+>) emptyWidget $ do
  col <- [0 .. (width - 1)]
  let background = overlay (1 - fromIntegral col / fromIntegral width) (0, 0, 0) color
  return (withColor background background (str "h"))
