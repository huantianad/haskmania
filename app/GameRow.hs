module GameRow (RowElement (Block), drawRow, RgbColor, Orientation (Vertical, Horizontal)) where

import Brick (Widget, hBox, modifyDefAttr, str, vBox)
import Data.Maybe (fromMaybe, listToMaybe)
import Graphics.Vty (Color (RGBColor), withBackColor, withForeColor)

type RgbColor = (Float, Float, Float)

-- Block: position length color
data RowElement = Block Float Float RgbColor

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

blockChar :: Orientation -> Float -> Char
blockChar orientation fraction = case orientation of
  Vertical -> " ▁▂▃▄▅▆▇█" !! index
  Horizontal -> " ▏▎▍▌▋▊▉█" !! index
  where
    index = floor (fraction * 8) `mod` 8

drawRow :: Orientation -> Int -> Float -> RgbColor -> [RowElement] -> Widget ()
drawRow orientation size offset rowColor elements = combine $ do
  i <- [0 .. (fromIntegral size - 1)]
  let progress = i / fromIntegral size
  let progress' =
        case orientation of
          Horizontal -> 1 - progress
          Vertical -> progress
  let background = overlay (progress' * 0.3 + 0.05) (0, 0, 0) rowColor
  let right = listToMaybe $ do
        Block pos len color <- elements
        if i + offset + 1 > pos && i + offset + 1 <= pos + len
          then return (max 0 (pos - (i + offset)), color)
          else []
  let left = listToMaybe $ do
        Block pos len color <- elements
        if i + offset >= pos + len - 1 && i + offset < pos + len
          then return (pos + len - (i + offset), color)
          else []
  return $ case (left, right) of
    (Nothing, Nothing) -> withColor background background (str " ")
    (Nothing, Just (fraction, color)) -> withColor color background (str [blockChar orientation fraction])
    (Just (fraction, color), Nothing) -> withColor background color (str [blockChar orientation fraction])
    -- In the case of a conflict, I'm arbitrarily letting the left one win
    (Just (fraction, leftColor), Just (_, rightColor)) -> withColor rightColor leftColor (str [blockChar orientation fraction])
  where
    combine = case orientation of
      Horizontal -> hBox
      Vertical -> vBox
