module HaskMania.Feedback (Feedback (Notice), Time, drawFeedback, feedbackVisible) where

import Brick (Widget, cropLeftBy, cropRightBy, emptyWidget, modifyDefAttr, str, (<+>))
import Brick.Widgets.Core ((<=>))
import Data.List (intersperse)
import Graphics.Vty (bold, withStyle)
import HaskMania.Color (applyAlpha, overlay, withColor)
import HaskMania.GameRow (RgbColor)

type Time = Double

-- | `Notice` takes color, text content, start time
data Feedback = Notice RgbColor String Time

-- | AnimateIn goes from 1 to 0
-- | AnimateOut goes from 0 to 1
data AnimationState = Visible | AnimateIn Double | AnimateOut Double | Hidden
  deriving (Eq, Show)

background :: RgbColor
background = (0, 0, 0)

duration :: Time
duration = 1

transition :: Time
transition = 0.3

-- | i copy from https://sheeptester.github.io/javascripts/easing.html
easeInOutCubic :: Double -> Double
easeInOutCubic t =
  let t' = t * 2
   in if t' < 1
        then t' * t' * t' / 2
        else
          let t'' = t' - 2
           in (t'' * t'' * t'' + 2) / 2

-- | t is relative to the animation start time
getAnimationState :: Time -> AnimationState
getAnimationState t
  | t >= 0 && t < transition = AnimateIn (1 - t / transition)
  | t >= duration - transition && t < duration = AnimateOut (1 - (duration - t) / transition)
  | t >= 0 && t < duration = Visible
  | otherwise = Hidden

-- | this will be useful for clearing feedback that is done
feedbackVisible :: Time -> Feedback -> Bool
feedbackVisible t (Notice _ _ start) = getAnimationState (t - start) == Hidden

drawFeedback :: Time -> Feedback -> Widget ()
drawFeedback t (Notice color text start) =
  case getAnimationState (t - start) of
    Hidden -> emptyWidget
    Visible -> box
    AnimateIn progress -> cropLeftBy (floor $ totalWidth * easeInOutCubic progress) box
    AnimateOut progress -> cropRightBy (floor $ totalWidth * easeInOutCubic progress) box
  where
    spacedText = intersperse ' ' text
    totalWidth = fromIntegral (length spacedText + 6)

    bgColor = overlay background (applyAlpha 0.1 color)
    borderColor = overlay background (applyAlpha 0.5 color)
    withTextColor = withColor bgColor color
    withBorderColor = withColor bgColor borderColor

    border = withBorderColor (str ('═' <$ spacedText))
    box =
      withBorderColor (str " ╔═\n ║ \n ╚═")
        <+> ( border
                <=> withTextColor (modifyDefAttr (`withStyle` bold) (str spacedText))
                <=> border
            )
        <+> withBorderColor (str "═╗ \n ║ \n═╝ ")
