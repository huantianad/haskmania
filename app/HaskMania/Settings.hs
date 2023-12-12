module HaskMania.Settings
  ( defaultSettings,
    MySettings,
    volumeUpKey,
    volumeDownKey,
    columnOneKey,
    columnTwoKey,
    columnThreeKey,
    columnFourKey,
    changeKey,
  )
where

import Control.Monad.State.Class (MonadState (put))
import Graphics.Vty qualified as V
import Lens.Micro.Platform (ASetter', makeLenses, (&), (.~))

-- Your module code here
data MySettings = MySettings
  { _volumeUpKey :: V.Key,
    _volumeDownKey :: V.Key,
    _columnOneKey :: V.Key,
    _columnTwoKey :: V.Key,
    _columnThreeKey :: V.Key,
    _columnFourKey :: V.Key
  }

makeLenses ''MySettings

defaultSettings :: MySettings
defaultSettings =
  MySettings
    { _volumeUpKey = V.KUp,
      _volumeDownKey = V.KDown,
      -- vim keybindings
      _columnOneKey = V.KChar 'h',
      _columnTwoKey = V.KChar 'j',
      _columnThreeKey = V.KChar 'k',
      _columnFourKey = V.KChar 'l'
    }

changeKey :: (MonadState MySettings m) => ASetter' MySettings V.Key -> V.Key -> MySettings -> m ()
-- changeKey keyField newKey settings = settings & keyField .~ newKey
changeKey keyField newKey settings = do
  put $ settings & keyField .~ newKey
