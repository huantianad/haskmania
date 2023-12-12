{-# LANGUAGE RecordWildCards #-}

module HaskMania.SoundW (SoundW (soundPaused), soundPlay, togglePause, soundPos, volumeAdjust) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class (MonadState (get, put))
import Sound.ProteaAudio qualified as PA

data SoundW = SoundW
  { wrappedSound :: PA.Sound,
    soundPaused :: Bool,
    volume :: Float
  }

soundPlay :: PA.Sample -> IO SoundW
soundPlay s = do
  sound <- PA.soundPlay s 1 1 0 1
  return $
    SoundW
      { wrappedSound = sound,
        soundPaused = False,
        volume = 1
      }

soundPos :: SoundW -> IO Double
soundPos = PA.soundPos . wrappedSound

togglePause :: (MonadState SoundW m, MonadIO m) => m ()
togglePause = do
  sw@SoundW {..} <- get

  result <- liftIO $ PA.soundUpdate wrappedSound (not soundPaused) volume volume 0 1
  unless result $ liftIO $ fail "Failed to toggle sound state"

  put $ sw {soundPaused = not soundPaused}

volumeAdjust :: (MonadState SoundW m, MonadIO m) => String -> m ()
volumeAdjust dir = do
  sw@SoundW {..} <- get

  let nv = case dir of
        "up" -> min 1 (volume + 0.1)
        "down" -> max 0 (volume - 0.1)
        _ -> volume

  result <- liftIO $ PA.soundUpdate wrappedSound soundPaused nv nv 0 1
  unless result $ liftIO $ fail "Failed to adjust volume"
  put $ sw {volume = nv}
