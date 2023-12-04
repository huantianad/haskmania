{-# LANGUAGE RecordWildCards #-}

module HaskMania.LinearRegression (initRegression, updateTime, getTime) where

import Data.Time.Clock.System (SystemTime (..), getSystemTime)
import Sound.ProteaAudio (Sound, soundPos)

data RegressionModel = RegressionModel
  { points :: [(Double, Double)],
    slope :: Double,
    intercept :: Double,
    previouslyReturnedTime :: Double
  }

initRegression :: Sound -> IO RegressionModel
initRegression = updateTime empty
  where
    empty =
      RegressionModel
        { points = [],
          slope = 0,
          intercept = 0,
          previouslyReturnedTime = 0
        }

updateTime :: RegressionModel -> Sound -> IO RegressionModel
updateTime model sound = do
  let paused = False -- TODO: Get paused status from sound instead
  x <- toSeconds <$> getSystemTime
  y <- soundPos sound
  return $ addPoint model (x, y) paused

toSeconds :: SystemTime -> Double
toSeconds (MkSystemTime {systemSeconds = s, systemNanoseconds = n}) = fromIntegral s + fromIntegral n * 1_000_000

getTime :: RegressionModel -> Sound -> IO (Double, RegressionModel)
getTime model@RegressionModel {..} sound = do
  let paused = False -- TODO: Get paused status from sound instead
  x <- toSeconds <$> getSystemTime
  let time = x * slope + intercept

  if (time < previouslyReturnedTime) || paused
    then return (previouslyReturnedTime, model)
    else return (time, model {previouslyReturnedTime = time})

-- Once the number of points in the regression reaches this limit,
-- older points will be dropped and replaced with the newly added points.
limit :: Int
limit = 20

addPoint ::
  RegressionModel ->
  (Double, Double) ->
  Bool ->
  RegressionModel
-- Clear points list if game is paused
addPoint model _ True = model {points = []}
addPoint model@RegressionModel {..} point@(_, y) False =
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

updateRegression :: RegressionModel -> RegressionModel
updateRegression model@RegressionModel {..} =
  if length points == 1
    then
      model
        { slope = 0,
          intercept = fst $ head points
        }
    else
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
