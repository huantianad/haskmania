module HaskMania.PauseScreen (pauseScreen) where

import Brick (Widget, str)
import Brick.Widgets.Center (center)

pauseScreen :: Widget ()
pauseScreen = center $ str "Paused\nPress Enter to exit\nPress Esc to resume"
