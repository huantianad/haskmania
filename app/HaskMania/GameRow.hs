module HaskMania.GameRow (RowElement (Block), drawRow, RgbColor, RgbaColor, Orientation (Vertical, Horizontal)) where

import Brick (Widget, hBox, modifyDefAttr, str, vBox)
import Graphics.Vty (Color (RGBColor), withBackColor, withForeColor)

type RgbColor = (Double, Double, Double)

type RgbaColor = (Double, Double, Double, Double)

-- Block: color length position
data RowElement = Block RgbaColor Double Double

data Orientation = Vertical | Horizontal

toRGBColor :: RgbColor -> Color
toRGBColor (r, g, b) = RGBColor (floor r) (floor g) (floor b)

-- Applies a transparent color on top of a base color
overlay :: RgbaColor -> RgbColor -> RgbColor
overlay (r', g', b', alpha) (r, g, b) =
  ( r * (1 - alpha) + r' * alpha,
    g * (1 - alpha) + g' * alpha,
    b * (1 - alpha) + b' * alpha
  )

mixAlpha :: Double -> RgbColor -> RgbaColor
mixAlpha alpha (r, g, b) = (r, g, b, alpha)

withColor :: RgbColor -> RgbColor -> Widget n -> Widget n
withColor back fore = modifyDefAttr (\a -> withBackColor (withForeColor a (toRGBColor fore)) (toRGBColor back))

blockChar :: Orientation -> Double -> Char
blockChar orientation fraction = case orientation of
  Vertical -> " ▁▂▃▄▅▆▇█" !! index
  Horizontal -> " ▏▎▍▌▋▊▉█" !! index
  where
    index = floor (fraction * 8) `mod` 8

findElement :: Double -> (RowElement -> Maybe a) -> [RowElement] -> Maybe a
findElement _ _ [] = Nothing
findElement stop predicate (element@(Block _ _ pos) : rest)
  | pos > stop = Nothing
  | otherwise = case predicate element of
      Just x -> Just x
      Nothing -> findElement stop predicate rest

-- `elements` should be sorted
drawRow :: Orientation -> Int -> Double -> RgbColor -> [RowElement] -> Maybe Char -> Widget ()
drawRow orientation size offset rowColor elements char = combine $ do
  i <- case orientation of
    Horizontal -> [0 .. (fromIntegral size - 1)]
    Vertical -> reverse [0 .. (fromIntegral size - 1)]
  let background =
        overlay (255, 255, 255, if i == 2 then 0.5 else 0) $
          overlay (mixAlpha ((1 - i / fromIntegral size) * 0.3 + 0.05) rowColor) (0, 0, 0)
  case (i, char) of
    (0, Just c) -> return $ withColor background rowColor (str [c])
    _ -> do
      let right =
            findElement
              (offset + fromIntegral size)
              ( \(Block color len pos) ->
                  if i + offset + 1 > pos && i + offset + 1 <= pos + len
                    then Just (max 0 (pos - (i + offset)), color)
                    else Nothing
              )
              elements
      let left =
            findElement
              (offset + fromIntegral size)
              ( \(Block color len pos) ->
                  if i + offset >= pos + len - 1 && i + offset < pos + len
                    then Just (pos + len - (i + offset), color)
                    else Nothing
              )
              elements
      return $ case (left, right) of
        (Nothing, Nothing) -> withColor background background (str " ")
        (Nothing, Just (fraction, color)) -> withColor (overlay color background) background (str [blockChar orientation fraction])
        (Just (fraction, color), Nothing) -> withColor background (overlay color background) (str [blockChar orientation fraction])
        -- In the case of a conflict, I'm arbitrarily letting the left one win
        (Just (fraction, leftColor), Just (_, rightColor)) -> withColor (overlay rightColor background) (overlay leftColor background) (str [blockChar orientation fraction])
  where
    combine = case orientation of
      Horizontal -> hBox
      Vertical -> vBox
