module HaskMania.PauseScreen (pauseScreen) where

import Brick (Widget, modifyDefAttr, str, (<=>))
import Brick.Widgets.Center (hCenter, vCenter)
import Graphics.Vty.Attributes (bold, withStyle)

pauseScreen :: Widget ()
pauseScreen =
  vCenter
    ( hCenter (modifyDefAttr (`withStyle` bold) (str "Paused"))
        <=> hCenter (str "Press Enter to exit\nPress Esc to resume")
    )
