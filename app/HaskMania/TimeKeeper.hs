{-# LANGUAGE RecordWildCards #-}

module HaskMania.TimeKeeper (TimeKeeper, initTimeKeeper, updateTimeM, getTimeM) where

import Control.Monad.Cont (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState (..))
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Sound.ProteaAudio (Sound, soundPos)

data TimeKeeper = TimeKeeper
  { points :: [(Double, Double)],
    slope :: Double,
    intercept :: Double,
    previouslyReturnedTime :: Double,
    sound :: Sound
  }

-- TODO: Implement this stub
soundPaused :: Sound -> IO Bool
soundPaused _ = return False

toSeconds :: SystemTime -> Double
toSeconds (MkSystemTime {systemSeconds = s, systemNanoseconds = n}) = fromIntegral s + fromIntegral n * 1_000_000

initTimeKeeper :: Sound -> IO TimeKeeper
initTimeKeeper sound =
  updateTime
    TimeKeeper
      { points = [],
        slope = 0,
        intercept = 0,
        previouslyReturnedTime = 0,
        sound = sound
      }

updateTime :: TimeKeeper -> IO TimeKeeper
updateTime model = do
  x <- toSeconds <$> getSystemTime
  y <- soundPos $ sound model

  paused <- soundPaused $ sound model

  -- If the audio is paused, this invalidates our previous points
  -- Remove all of them so we can restart from scratch.
  return $
    if paused
      then model {points = []}
      else addPoint model (x, y)

updateTimeM :: (MonadState TimeKeeper m, MonadIO m) => m ()
updateTimeM = get >>= (liftIO . updateTime) >>= put

getTime :: TimeKeeper -> IO (Double, TimeKeeper)
getTime model@TimeKeeper {..} = do
  x <- toSeconds <$> getSystemTime
  let time = x * slope + intercept
  paused <- soundPaused sound

  if (time < previouslyReturnedTime) || paused
    then return (previouslyReturnedTime, model)
    else return (time, model {previouslyReturnedTime = time})

getTimeM :: (MonadState TimeKeeper m, MonadIO m) => m Double
getTimeM = do
  tk <- get
  (time, tk') <- liftIO $ getTime tk
  put tk'
  return time

-- Once the number of points in the regression reaches this limit,
-- older points will be dropped and replaced with the newly added points.
limit :: Int
limit = 20

addPoint :: TimeKeeper -> (Double, Double) -> TimeKeeper
addPoint model@TimeKeeper {..} point@(_, y) =
  -- Don't add multiple system times for the same audio time
  if any (\p -> snd p == y) points
    then model
    else updateRegression model {points = points'}
  where
    -- Trim points if we would go past the limit
    points' =
      if length points == limit
        then point : init points
        else point : points

updateRegression :: TimeKeeper -> TimeKeeper
updateRegression model@TimeKeeper {..} =
  case points of
    [point] ->
      model
        { slope = 1,
          intercept = snd point - fst point
        }
    _ ->
      model
        { slope = slope',
          intercept = yMean - slope' * xMean
        }
  where
    average :: (Fractional a) => [a] -> a
    average xs = sum xs / fromIntegral (length xs)

    xMean = average $ map fst points
    yMean = average $ map snd points
    slopeNumerator = sum $ map (\p -> (fst p - xMean) * (snd p - yMean)) points
    slopeDenominator = sum $ map (\p -> (fst p - xMean) * (fst p - xMean)) points
    slope' = slopeNumerator / slopeDenominator
