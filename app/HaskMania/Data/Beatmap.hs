{-# LANGUAGE DuplicateRecordFields #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HaskMania.Data.Beatmap where

import Data.Map (Map)

type Position = (Int, Int)

type Color = (Int, Int, Int)

newtype Time = Milliseconds Int

newtype Percent = Percent Int

data Beatmap = Beatmap
  { general :: BeatmapInfo,
    editor :: BeatmapEditorSettings,
    metadata :: BeatmapMetadata,
    difficulty :: BeatmapDifficulty,
    events :: [BeatmapEvent],
    timings :: [TimingPoint],
    colors :: BeatmapColors,
    objects :: [HitObject]
  }

data BeatmapInfo = BeatmapInfo
  { audioFilename :: String,
    audioLeadIn :: Time,
    previewTime :: Time,
    countdown :: Float,
    sampleSet :: String,
    stackLeniency :: Float,
    mode :: Int,
    showLetterboxInBreaks :: Bool,
    useSkinSprites :: Bool,
    overlayPosition :: OverlayPosition,
    preferredSkin :: String,
    showEpilepsyWarning :: Bool,
    countdownOffset :: Int,
    useSpecialStyle :: Bool,
    storyboardSupportsWidescreen :: Bool,
    samplesMatchPlaybackRate :: Bool
  }

data OverlayPosition = Default | Below | Above

data BeatmapEditorSettings = BeatmapEditorSettings
  { bookmarks :: [Int],
    distanceSpacing :: Float,
    beatDivisor :: Int,
    gridSize :: Int,
    timelineZoom :: Float
  }

data BeatmapMetadata = BeatmapMetadata
  { title :: String,
    titleRomanized :: String,
    artist :: String,
    artistRomanized :: String,
    creator :: String,
    version :: String,
    source :: String,
    tags :: [String],
    id :: Int,
    setId :: Int
  }

data BeatmapDifficulty = BeatmapDifficulty
  { hpDrainRate :: Float,
    circleSize :: Float,
    overallDifficulty :: Float,
    approachRate :: Float,
    sliderBaseVelocity :: Float,
    sliderTicksPerBeat :: Float
  }

data BeatmapEvent = BeatmapEvent

data TimingPoint = TimingPoint
  { time :: Time,
    beatLength :: Float,
    meter :: Int,
    sampleSet :: Int,
    sampleIndex :: Int,
    volume :: Percent,
    isInherited :: Bool,
    effects :: TimingPointEffects
  }

data TimingPointEffects = TimingPointEffects
  { isKiaiTime :: Bool,
    omitFirstBarline :: Bool
  }

data BeatmapColors = BeatmapColors
  { combos :: Map Int Color,
    sliderTrackOverride :: Color,
    sliderBorder :: Color
  }

data HitObject = HitObject
  { position :: Position,
    time :: Time,
    kind :: HitObjectKind,
    sounds :: HitSounds,
    sample :: HitSample
  }

data HitObjectKind
  = Circle CircleParams
  | Slider SliderParams
  | Spinner SpinnerParams
  | Hold HoldParams

data SliderParams = SliderParams
  { curveType :: SliderCurveType,
    points :: [Position],
    numSlides :: Int,
    length :: Float,
    edgeSounds :: [Int],
    edgeSets :: [String]
  }

data CircleParams = CircleParams

data SpinnerParams = SpinnerParams {endTime :: Time}

data HoldParams = HoldParams {endTime :: Time}

data SliderCurveType = Linear | Bezier | Catmull | Circular

data HitSounds = HitSounds
  { normal :: Bool,
    whistle :: Bool,
    finish :: Bool,
    clap :: Bool
  }

data HitSample = HitSample
  { normalSet :: Int,
    additionSet :: Int,
    index :: Int,
    volume :: Percent,
    filename :: String
  }
