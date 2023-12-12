{-# LANGUAGE RecordWildCards #-}

module HaskMania.SoundW (SoundW (soundPaused), VolumeDir (..), soundPlay, togglePause, soundPos, volumeAdjust) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class (MonadState (get), modify)
import Sound.ProteaAudio qualified as PA

data SoundW = SoundW
  { wrappedSound :: PA.Sound,
    soundPaused :: Bool,
    volume :: Float
  }

data VolumeDir = Increase | Decrease

soundPlay :: PA.Sample -> IO SoundW
soundPlay s = do
  sound <- PA.soundPlay s 1 1 0 1
  return $
    SoundW
      { wrappedSound = sound,
        soundPaused = False,
        volume = 1
      }

-- Calls PA.soundUpdate on the wrapped sound, so that the state of the sound
-- matches the current state of the settings in the wrapper.
soundUpdate :: (MonadState SoundW m, MonadIO m) => m ()
soundUpdate = do
  SoundW {..} <- get
  result <- liftIO $ PA.soundUpdate wrappedSound soundPaused volume volume 0 1
  unless result $ liftIO $ fail "Failed to toggle sound state"

soundPos :: SoundW -> IO Double
soundPos = PA.soundPos . wrappedSound

togglePause :: (MonadState SoundW m, MonadIO m) => m ()
togglePause = do
  modify $ \sw -> sw {soundPaused = not $ soundPaused sw}
  soundUpdate

volumeAdjust :: (MonadState SoundW m, MonadIO m) => VolumeDir -> m ()
volumeAdjust dir = do
  let volumeModifier = case dir of
        Increase -> min 1 . (+ 0.1)
        Decrease -> max 0 . subtract 0.1

  modify $ \sw -> sw {volume = volumeModifier $ volume sw}
  soundUpdate
