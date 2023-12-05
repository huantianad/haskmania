module HaskMania.GameRow (RowElement (Block), drawRow, RgbColor, Orientation (Vertical, Horizontal)) where

import Brick (Widget, hBox, modifyDefAttr, str, vBox)
import Data.Maybe (fromMaybe, listToMaybe)
import Graphics.Vty (Color (RGBColor), withBackColor, withForeColor)

type RgbColor = (Float, Float, Float)

-- Block: color length position
data RowElement = Block RgbColor Float Float

data Orientation = Vertical | Horizontal

toRGBColor :: RgbColor -> Color
toRGBColor (r, g, b) = RGBColor (floor r) (floor g) (floor b)

-- Applies a transparent color on top of a base color
overlay :: Float -> RgbColor -> RgbColor -> RgbColor
overlay alpha (r', g', b') (r, g, b) =
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
  i <- case orientation of
    Horizontal -> [0 .. (fromIntegral size - 1)]
    Vertical -> reverse [0 .. (fromIntegral size - 1)]
  let background =
        overlay (if i == 1 then 0.5 else 0) (255, 255, 255) $
          overlay ((1 - i / fromIntegral size) * 0.3 + 0.05) rowColor (0, 0, 0)
  let right = listToMaybe $ do
        Block color len pos <- elements
        if i + offset + 1 > pos && i + offset + 1 <= pos + len
          then return (max 0 (pos - (i + offset)), color)
          else []
  let left = listToMaybe $ do
        Block color len pos <- elements
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
