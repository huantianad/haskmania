{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module HaskMania.Data.Parser (openBeatmapSet, readFileFromBeatmapSet) where

import Codec.Archive.Zip.Conduit.UnZip
import Conduit
import Control.Applicative (Applicative (liftA2), (<|>))
import Control.Applicative.Permutations
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Text qualified as APT
import Data.Bits ((.&.))
import Data.ByteString.Char8 qualified as BS hiding (takeWhile)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Functor
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding as TE
import Data.Word (Word8)
import HaskMania.Data.Beatmap qualified as B
import Prelude hiding (length, takeWhile)

openBeatmapSet :: (MonadResource m, PrimMonad m, MonadThrow m, MonadFail m) => FilePath -> ConduitT () B.Beatmap m ()
openBeatmapSet path =
  sourceFile path
    .| fmap snd (fuseBoth unZipStream readBeatmaps)
  where
    readBeatmaps = awaitForever readBeatmap

    readBeatmap (Left entry) = do
      if isBeatmap entry
        then do
          bm <- write .| sinkParser beatmap
          yield bm
        else write .| sinkNull
      return ()
    readBeatmap (Right _) = fail "unexpected contents"

    isBeatmap ZipEntry {zipEntryName} = either (T.isSuffixOf $ T.pack ".osu") (BS.isSuffixOf $ BS.pack ".osu") zipEntryName

    write = await >>= maybe (return ()) block
    block (Right r) = yield r >> write
    block l = leftover l

readFileFromBeatmapSet :: (MonadResource m, MonadThrow m, PrimMonad m, MonadFail m) => FilePath -> T.Text -> ConduitT a BS.ByteString m ()
readFileFromBeatmapSet path filename =
  sourceFile path
    .| fmap snd (fuseBoth unZipStream readFiles)
  where
    readFiles = awaitForever readF

    readF (Left ZipEntry {zipEntryName}) = do
      if either (== filename) (== TE.encodeUtf8 filename) zipEntryName
        then do
          write
        else do
          write .| sinkNull
      return ()
    readF (Right _) = fail "unexpected contents"

    write = await >>= maybe (return ()) block
    block (Right r) = yield r >> write
    block l = leftover l

beatmap :: Parser B.Beatmap
beatmap = do
  ver <- header
  runPermutation $
    cons ver
      <$> toPermutation beatmapInfo
      <*> toPermutation beatmapEditor
      <*> toPermutation beatmapMetadata
      <*> toPermutation beatmapDifficulty
      <*> toPermutation beatmapEvents
      <*> toPermutation beatmapTimings
      <*> toPermutation beatmapColors
      <*> toPermutation beatmapObjects
  where
    cons
      formatVersion
      info
      editor
      metadata
      difficulty
      events
      timings
      colors
      objects =
        B.Beatmap
          { B.formatVersion,
            B.info,
            B.editor,
            B.metadata,
            B.difficulty,
            B.events,
            B.timings,
            B.colors,
            B.objects
          }

literal :: String -> Parser BS.ByteString
literal = string . BS.pack

header :: Parser Int
header = do
  void $ literal "osu file format v"
  res <- decimal
  void skipSpace
  return res

sectionHeader :: String -> Parser ()
sectionHeader name = do
  void $ char '['
  void $ literal name
  void $ char ']'
  skipSpace

colon :: Parser ()
colon = char ':' $> ()

colonspace :: Parser ()
colonspace = colon >> char ' ' $> ()

spacecolonspace :: Parser ()
spacecolonspace = char ' ' >> colonspace

parseKV :: Parser () -> String -> Parser a -> Parser a
parseKV sep key parser = do
  void $ literal key
  sep
  res <- parser
  skipSpace
  return res

(<||>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

bool :: Parser Bool
bool = (char '0' $> False) <|> (char '1' $> True)

str :: Parser T.Text
str = takeTill APT.isEndOfLine <&> TE.decodeUtf8

time :: Parser B.Time
time = signed decimal <&> B.Milliseconds

percent :: Parser B.Percent
percent = decimal <&> B.Percent

strList :: Char -> (Char -> Bool) -> Parser [T.Text]
strList sep exclude = item `sepBy` char sep
  where
    item = takeTill ((== sep) <||> exclude <||> APT.isEndOfLine) <&> TE.decodeUtf8

overlayPos :: Parser B.OverlayPosition
overlayPos =
  (literal "NoChange" $> B.Default)
    <|> (literal "Below" $> B.Below)
    <|> (literal "Above" $> B.Above)

beatmapInfo :: Parser B.BeatmapInfo
beatmapInfo =
  do
    void $ sectionHeader "General"
    runPermutation $
      cons
        <$> kv "AudioFilename" str
        <*> kvDef "AudioLeadIn" time (B.Milliseconds 0)
        <*> kvDef "PreviewTime" time (B.Milliseconds (-1))
        <*> kvDef "Countdown" decimal 1
        <*> kvDef "SampleSet" str (T.pack "Normal")
        <*> kvDef "StackLeniency" double 0.7
        <*> kvDef "Mode" decimal 0
        <*> kvDef "LetterboxInBreaks" bool False
        <*> kvDef "UseSkinSprites" bool False
        <*> kvDef "OverlayPosition" overlayPos B.Default
        <*> kvDef "SkinPreference" str (T.pack "")
        <*> kvDef "EpilepsyWarning" bool False
        <*> kvDef "CountdownOffset" decimal 0
        <*> kvDef "SpecialStyle" bool False
        <*> kvDef "WidescreenStoryboard" bool False
        <*> kvDef "SamplesMatchPlaybackRate" bool False
  where
    kvP = parseKV colonspace
    kv k v = toPermutation $ kvP k v
    kvDef k v def = toPermutationWithDefault def $ kvP k v

    cons
      audioFilename
      audioLeadIn
      previewTime
      countdown
      sampleSet
      stackLeniency
      mode
      showLetterboxInBreaks
      useSkinSprites
      overlayPosition
      preferredSkin
      showEpilepsyWarning
      countdownOffset
      useSpecialStyle
      storyboardSupportsWidescreen
      samplesMatchPlaybackRate =
        B.BeatmapInfo
          { B.audioFilename,
            B.audioLeadIn,
            B.previewTime,
            B.countdown,
            B.sampleSet,
            B.stackLeniency,
            B.mode,
            B.showLetterboxInBreaks,
            B.useSkinSprites,
            B.overlayPosition,
            B.preferredSkin,
            B.showEpilepsyWarning,
            B.countdownOffset,
            B.useSpecialStyle,
            B.storyboardSupportsWidescreen,
            B.samplesMatchPlaybackRate
          }

beatmapEditor :: Parser B.BeatmapEditorSettings
beatmapEditor = do
  void $ sectionHeader "Editor"
  runPermutation $
    cons
      <$> kvDef "Bookmarks" (time `sepBy` char ',') []
      <*> kv "DistanceSpacing" double
      <*> kv "BeatDivisor" decimal
      <*> kv "GridSize" decimal
      <*> kv "TimelineZoom" double
  where
    kvP = parseKV colonspace
    kv k v = toPermutation $ kvP k v
    kvDef k v def = toPermutationWithDefault def $ kvP k v

    cons
      bookmarks
      distanceSpacing
      beatDivisor
      gridSize
      timelineZoom =
        B.BeatmapEditorSettings
          { B.bookmarks,
            B.distanceSpacing,
            B.beatDivisor,
            B.gridSize,
            B.timelineZoom
          }

beatmapMetadata :: Parser B.BeatmapMetadata
beatmapMetadata = do
  void $ sectionHeader "Metadata"
  runPermutation $
    cons
      <$> kv "TitleUnicode" str
      <*> kv "Title" str
      <*> kv "ArtistUnicode" str
      <*> kv "Artist" str
      <*> kv "Creator" str
      <*> kv "Version" str
      <*> kv "Source" str
      <*> kv "Tags" (strList ' ' (const False))
      <*> kv "BeatmapID" decimal
      <*> kv "BeatmapSetID" decimal
  where
    kvP = parseKV colon
    kv k v = toPermutation $ kvP k v

    cons
      title
      titleRomanized
      artist
      artistRomanized
      creator
      version
      source
      tags
      beatmapId
      setId =
        B.BeatmapMetadata
          { B.title,
            B.titleRomanized,
            B.artist,
            B.artistRomanized,
            B.creator,
            B.version,
            B.source,
            B.tags,
            B.beatmapId,
            B.setId
          }

beatmapDifficulty :: Parser B.BeatmapDifficulty
beatmapDifficulty = do
  void $ sectionHeader "Difficulty"
  runPermutation $
    cons
      <$> kv "HPDrainRate" double
      <*> kv "CircleSize" double
      <*> kv "OverallDifficulty" double
      <*> kv "ApproachRate" double
      <*> kv "SliderMultiplier" double
      <*> kv "SliderTickRate" double
  where
    kvP = parseKV colon
    kv k v = toPermutation $ kvP k v

    cons
      hpDrainRate
      circleSize
      overallDifficulty
      approachRate
      sliderBaseVelocity
      sliderTicksPerBeat =
        B.BeatmapDifficulty
          { B.hpDrainRate,
            B.circleSize,
            B.overallDifficulty,
            B.approachRate,
            B.sliderBaseVelocity,
            B.sliderTicksPerBeat
          }

beatmapEvents :: Parser [B.BeatmapEvent]
beatmapEvents = do
  void $ sectionHeader "Events"
  -- currently unimplemented; skip to next section
  void $ takeTill (== '[')
  return []

beatmapTimings :: Parser [B.TimingPoint]
beatmapTimings = do
  void $ sectionHeader "TimingPoints"
  res <- timingPoint `sepBy` skipSpace
  skipSpace
  return res

timingPoint :: Parser B.TimingPoint
timingPoint = do
  startTime <- time
  void $ char ','
  beatLength <- double
  void $ char ','
  meter <- decimal
  void $ char ','
  sampleSet <- decimal
  void $ char ','
  sampleIndex <- decimal
  void $ char ','
  volume <- percent
  void $ char ','
  uninherited <- bool
  void $ char ','
  effects <- timingPointEffects

  return $
    B.TimingPoint
      { time = startTime,
        beatLength,
        meter,
        sampleSet,
        sampleIndex,
        volume,
        isInherited = not uninherited,
        effects
      }

timingPointEffects :: Parser B.TimingPointEffects
timingPointEffects = do
  b <- decimal :: Parser Word8
  return $
    B.TimingPointEffects
      { B.isKiaiTime = b .&. 0b00000001 /= 0,
        B.omitFirstBarline = b .&. 0b00001000 /= 0
      }

beatmapColors :: Parser B.BeatmapColors
beatmapColors = do
  void $ sectionHeader "Colours"
  combos <- many' $ combo sep
  runPermutation $
    cons
      combos
      <$> kvDef "SliderTrackOverride" (Just <$> color) Nothing
      <*> kvDef "SliderBorder" (Just <$> color) Nothing
  where
    sep = spacecolonspace
    kvP = parseKV sep
    kvDef k v def = toPermutationWithDefault def $ kvP k v

    cons
      combos
      sliderTrackOverride
      sliderBorder =
        B.BeatmapColors
          { B.combos = Map.fromList combos,
            B.sliderTrackOverride,
            B.sliderBorder
          }

color :: Parser B.Color
color = do
  r <- decimal
  void $ char ','
  g <- decimal
  void $ char ','
  b <- decimal

  return (r, g, b)

combo :: Parser () -> Parser (Int, B.Color)
combo sep = do
  void $ literal "Combo"
  index <- decimal
  sep
  color' <- color
  skipSpace

  return (index, color')

beatmapObjects :: Parser [B.HitObject]
beatmapObjects = do
  void $ sectionHeader "HitObjects"
  res <- hitObject `sepBy` skipSpace
  skipSpace
  return res

hitObject :: Parser B.HitObject
hitObject = do
  x <- decimal
  void $ char ','
  y <- decimal
  void $ char ','
  startTime <- time
  void $ char ','
  ty <- decimal :: Parser Word8
  void $ char ','
  sounds <- hitSounds
  void $ char ','
  (kind, sample) <- remainder ty

  return
    B.HitObject
      { B.position = (x, y),
        B.time = startTime,
        B.kind,
        B.sounds,
        B.sample
      }
  where
    remainder ty
      | ty .&. 0b00000001 /= 0 = do
          params <- circleParams
          sample <- option Nothing (Just <$> hitSample)
          return (B.Circle params, sample)
      | ty .&. 0b00000010 /= 0 = do
          params <- sliderParams
          sample <- option Nothing (char ',' >> Just <$> hitSample)
          return (B.Slider params, sample)
      | ty .&. 0b00001000 /= 0 = do
          params <- spinnerParams
          sample <- option Nothing (char ',' >> Just <$> hitSample)
          return (B.Spinner params, sample)
      | ty .&. 0b10000000 /= 0 = do
          params <- holdParams
          sample <- option Nothing (char ':' >> Just <$> hitSample)
          return (B.Hold params, sample)
      | otherwise = error $ "unknown hit object type: " ++ show ty

circleParams :: Parser B.CircleParams
circleParams = return B.CircleParams

sliderParams :: Parser B.SliderParams
sliderParams = do
  curveType <- ty
  void $ char '|'
  points <- point `sepBy` char '|'
  void $ char ','
  numSlides <- decimal
  void $ char ','
  length <- double
  edgeSounds <- option [] (char ',' >> decimal `sepBy` char '|')
  edgeSets <- option [] (char ',' >> strList '|' (== ','))

  return $
    B.SliderParams
      { B.curveType,
        B.points,
        B.numSlides,
        B.length,
        B.edgeSounds,
        B.edgeSets
      }
  where
    point = do
      x <- decimal
      void $ char ':'
      y <- decimal
      return (x, y)

    ty =
      (char 'L' $> B.Linear)
        <|> (char 'B' $> B.Bezier)
        <|> (char 'C' $> B.Catmull)
        <|> (char 'P' $> B.Circular)

spinnerParams :: Parser B.SpinnerParams
spinnerParams = do
  endTime <- time
  return $ B.SpinnerParams {B.endTime}

holdParams :: Parser B.HoldParams
holdParams = do
  endTime <- time
  return $ B.HoldParams {B.endTime}

hitSounds :: Parser B.HitSounds
hitSounds = do
  b <- decimal :: Parser Word8
  return $
    B.HitSounds
      { normal = b .&. 0b00000001 /= 0,
        whistle = b .&. 0b00000010 /= 0,
        finish = b .&. 0b00000100 /= 0,
        clap = b .&. 0b00001000 /= 0
      }

hitSample :: Parser B.HitSample
hitSample = do
  normalSet <- decimal
  void $ char ':'
  additionSet <- decimal
  void $ char ':'
  index <- decimal
  void $ char ':'
  volume <- percent
  void $ char ':'
  filename <- str

  return
    B.HitSample
      { B.normalSet,
        B.additionSet,
        B.index,
        B.volume,
        B.filename
      }
