{-# LANGUAGE RecordWildCards #-}

module HaskMania.TimeKeeper (TimeKeeper, initTimeKeeper, updateTime, getTime) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState (get, put), execStateT)
import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import HaskMania.SoundW (SoundW, soundPaused, soundPos)

data TimeKeeper = TimeKeeper
  { points :: [(Double, Double)],
    slope :: Double,
    intercept :: Double,
    startTime :: SystemTime,
    previouslyReturnedTime :: Double
  }

secondsSince :: SystemTime -> IO Double
secondsSince (MkSystemTime {systemSeconds = s, systemNanoseconds = n}) = do
  (MkSystemTime {systemSeconds = ns, systemNanoseconds = nn}) <- getSystemTime
  return $ (fromIntegral ns - fromIntegral s) + ((fromIntegral nn - fromIntegral n) / 1_000_000_000)

initTimeKeeper :: SoundW -> IO TimeKeeper
initTimeKeeper sound = do
  now <- getSystemTime
  execStateT
    (updateTime sound)
    TimeKeeper
      { points = [],
        slope = 0,
        intercept = 0,
        startTime = now,
        previouslyReturnedTime = 0
      }

updateTime :: (MonadState TimeKeeper m, MonadIO m) => SoundW -> m ()
updateTime sound = do
  model@TimeKeeper {..} <- get

  x <- liftIO $ secondsSince startTime
  y <- liftIO $ soundPos sound

  let paused = soundPaused sound

  -- If the audio is paused, this invalidates our previous points
  -- Remove all of them so we can restart from scratch.
  if paused
    then put $ model {points = []}
    else put $ addPoint model (x, y)

getTime :: (MonadState TimeKeeper m, MonadIO m) => SoundW -> m Double
getTime sound = do
  model@TimeKeeper {..} <- get

  x <- liftIO $ secondsSince startTime
  let time = x * slope + intercept

  let paused = soundPaused sound

  if (time < previouslyReturnedTime) || paused
    then return previouslyReturnedTime
    else do
      put $ model {previouslyReturnedTime = time}
      return time

-- Once the number of points in the regression reaches this limit,
-- older points will be dropped and replaced with the newly added points.
limit :: Int
limit = 200

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
