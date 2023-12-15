module HaskMania.GameRow (RowElement (Block), drawRow, RgbColor, RgbaColor, Orientation (Vertical, Horizontal)) where

import Brick (Widget, hBox, str, vBox)
import HaskMania.Color (RgbColor, RgbaColor, applyAlpha, overlay, withColor)

-- Block: color length position
data RowElement = Block RgbaColor Double Double

data Orientation = Vertical | Horizontal

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

-- | `elements` should be sorted
drawRow :: Orientation -> Int -> Double -> RgbColor -> [RowElement] -> Double -> Maybe Char -> Widget ()
drawRow orientation size offset rowColor elements targetOpacity char = combine $ do
  i <- case orientation of
    Horizontal -> [0 .. (fromIntegral size - 1)]
    Vertical -> reverse [0 .. (fromIntegral size - 1)]
  let background =
        (0, 0, 0) `overlay` applyAlpha ((1 - i / fromIntegral size) * 0.3 + 0.05) rowColor `overlay` (255, 255, 255, if i == 2 then targetOpacity else 0)
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
    (Nothing, Nothing) -> case (i, char) of
      (0, Just c) -> withColor background rowColor (str [c])
      _ -> withColor background background (str " ")
    (Nothing, Just (fraction, color)) -> withColor (overlay background color) background (str [blockChar orientation fraction])
    (Just (fraction, color), Nothing) -> withColor background (overlay background color) (str [blockChar orientation fraction])
    -- In the case of a conflict, I'm arbitrarily letting the left one win
    (Just (fraction, leftColor), Just (_, rightColor)) -> withColor (overlay background rightColor) (overlay background leftColor) (str [blockChar orientation fraction])
  where
    combine = case orientation of
      Horizontal -> hBox
      Vertical -> vBox
