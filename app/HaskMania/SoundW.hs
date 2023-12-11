{-# LANGUAGE RecordWildCards #-}

module HaskMania.SoundW (SoundW (soundPaused), soundPlay, togglePause, soundPos) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class (MonadState (get, put))
import Sound.ProteaAudio qualified as PA

data SoundW = SoundW
  { wrappedSound :: PA.Sound,
    soundPaused :: Bool
  }

soundPlay :: PA.Sample -> IO SoundW
soundPlay s = do
  sound <- PA.soundPlay s 1 1 0 1
  return $
    SoundW
      { wrappedSound = sound,
        soundPaused = False
      }

soundPos :: SoundW -> IO Double
soundPos = PA.soundPos . wrappedSound

togglePause :: (MonadState SoundW m, MonadIO m) => m ()
togglePause = do
  sw@SoundW {..} <- get

  result <- liftIO $ PA.soundUpdate wrappedSound (not soundPaused) 1 1 0 1
  unless result $ liftIO $ fail "Failed to toggle sound state"

  put $ sw {soundPaused = not soundPaused}
