module HaskMania.Color (RgbColor, RgbaColor, applyAlpha, overlay, toRGBColor, withColor) where

import Brick (Widget)
import Brick.Widgets.Core (modifyDefAttr)
import Graphics.Vty (Color (RGBColor), withBackColor, withForeColor)

type RgbColor = (Double, Double, Double)

type RgbaColor = (Double, Double, Double, Double)

toRGBColor :: RgbColor -> Color
toRGBColor (r, g, b) = RGBColor (floor r) (floor g) (floor b)

-- Applies a transparent color on top of a base color
overlay :: RgbColor -> RgbaColor -> RgbColor
overlay (r, g, b) (r', g', b', alpha) =
  ( r * (1 - alpha) + r' * alpha,
    g * (1 - alpha) + g' * alpha,
    b * (1 - alpha) + b' * alpha
  )

applyAlpha :: Double -> RgbColor -> RgbaColor
applyAlpha alpha (r, g, b) = (r, g, b, alpha)

-- | Order: background color, foreground color
withColor :: RgbColor -> RgbColor -> Widget n -> Widget n
withColor back fore = modifyDefAttr (\a -> withBackColor (withForeColor a (toRGBColor fore)) (toRGBColor back))
