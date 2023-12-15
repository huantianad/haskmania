{-# LANGUAGE DuplicateRecordFields #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HaskMania.Data.Beatmap where

import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8)

type Position = (Int, Int)

type Color = (Word8, Word8, Word8)

newtype Time = Milliseconds Int
  deriving (Show)

newtype Percent = Percent Int
  deriving (Show)

data Beatmap = Beatmap
  { formatVersion :: Int,
    info :: BeatmapInfo,
    editor :: BeatmapEditorSettings,
    metadata :: BeatmapMetadata,
    difficulty :: BeatmapDifficulty,
    events :: [BeatmapEvent],
    timings :: [TimingPoint],
    colors :: BeatmapColors,
    objects :: [HitObject]
  }
  deriving (Show)

data BeatmapInfo = BeatmapInfo
  { audioFilename :: Text,
    audioLeadIn :: Time,
    previewTime :: Time,
    countdown :: Int,
    sampleSet :: Text,
    stackLeniency :: Double,
    mode :: Int,
    showLetterboxInBreaks :: Bool,
    useSkinSprites :: Bool,
    overlayPosition :: OverlayPosition,
    preferredSkin :: Text,
    showEpilepsyWarning :: Bool,
    countdownOffset :: Int,
    useSpecialStyle :: Bool,
    storyboardSupportsWidescreen :: Bool,
    samplesMatchPlaybackRate :: Bool
  }
  deriving (Show)

data OverlayPosition = Default | Below | Above deriving (Show)

data BeatmapEditorSettings = BeatmapEditorSettings
  { bookmarks :: [Time],
    distanceSpacing :: Double,
    beatDivisor :: Int,
    gridSize :: Int,
    timelineZoom :: Double
  }
  deriving (Show)

data BeatmapMetadata = BeatmapMetadata
  { title :: Text,
    titleRomanized :: Text,
    artist :: Text,
    artistRomanized :: Text,
    creator :: Text,
    version :: Text,
    source :: Text,
    tags :: [Text],
    beatmapId :: Int,
    setId :: Int
  }
  deriving (Show)

data BeatmapDifficulty = BeatmapDifficulty
  { hpDrainRate :: Double,
    circleSize :: Double,
    overallDifficulty :: Double,
    approachRate :: Double,
    sliderBaseVelocity :: Double,
    sliderTicksPerBeat :: Double
  }
  deriving (Show)

data BeatmapEvent = BeatmapEvent deriving (Show)

data TimingPoint = TimingPoint
  { time :: Time,
    beatLength :: Double,
    meter :: Int,
    sampleSet :: Int,
    sampleIndex :: Int,
    volume :: Percent,
    isInherited :: Bool,
    effects :: TimingPointEffects
  }
  deriving (Show)

data TimingPointEffects = TimingPointEffects
  { isKiaiTime :: Bool,
    omitFirstBarline :: Bool
  }
  deriving (Show)

data BeatmapColors = BeatmapColors
  { combos :: Map Int Color,
    sliderTrackOverride :: Maybe Color,
    sliderBorder :: Maybe Color
  }
  deriving (Show)

data HitObject = HitObject
  { position :: Position,
    time :: Time,
    kind :: HitObjectKind,
    sounds :: HitSounds,
    sample :: HitSample
  }
  deriving (Show)

data HitObjectKind
  = Circle CircleParams
  | Slider SliderParams
  | Spinner SpinnerParams
  | Hold HoldParams
  deriving (Show)

data CircleParams = CircleParams
  deriving (Show)

data SliderParams = SliderParams
  { curveType :: SliderCurveType,
    points :: [Position],
    numSlides :: Int,
    length :: Double,
    edgeSounds :: [Int],
    edgeSets :: [Text]
  }
  deriving (Show)

data SpinnerParams = SpinnerParams {endTime :: Time}
  deriving (Show)

data HoldParams = HoldParams {endTime :: Time}
  deriving (Show)

data SliderCurveType = Linear | Bezier | Catmull | Circular
  deriving (Show)

data HitSounds = HitSounds
  { normal :: Bool,
    whistle :: Bool,
    finish :: Bool,
    clap :: Bool
  }
  deriving (Show)

data HitSample = HitSample
  { normalSet :: Int,
    additionSet :: Int,
    index :: Int,
    volume :: Percent,
    filename :: Text
  }
  deriving (Show)
