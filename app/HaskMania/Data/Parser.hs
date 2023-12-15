{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module HaskMania.Data.Parser (openBeatmapSet) where

import Codec.Archive.Zip.Conduit.UnZip
import Conduit
import Control.Applicative ((<|>))
import Control.Applicative.Permutations
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as BS hiding (takeWhile)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Functor
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding as TE
import Data.Text.IO qualified as TIO
import HaskMania.Data.Beatmap qualified as B
import Prelude hiding (takeWhile)

openBeatmapSet :: (MonadResource m, PrimMonad m, MonadThrow m, MonadFail m) => FilePath -> ConduitT () B.Beatmap m ()
openBeatmapSet path =
  sourceFile path
    .| fmap snd (fuseBoth unZipStream readBeatmaps)
  where
    readBeatmaps = awaitForever readBeatmap

    readBeatmap (Left entry@ZipEntry {zipEntryName}) = do
      liftIO $ either TIO.putStrLn BS.putStrLn zipEntryName
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

beatmap :: Parser B.Beatmap
beatmap = do
  ver <- header
  runPermutation $
    ctor ver
      <$> toPermutation beatmapInfo
      <*> toPermutation beatmapEditor
      <*> toPermutation beatmapMetadata
      <*> toPermutation beatmapDifficulty
      <*> toPermutation beatmapEvents
      <*> toPermutation beatmapTimings
      <*> toPermutation beatmapColors
      <*> toPermutation beatmapObjects
  where
    ctor
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
colonspace = colon $> char ' ' $> ()

spacecolonspace :: Parser ()
spacecolonspace = char ' ' $> colonspace $> ()

parseKV :: Parser () -> String -> Parser a -> Parser a
parseKV sep key parser = do
  void $ literal key
  void sep
  res <- parser
  void skipSpace
  return res

bool :: Parser Bool
bool = (char '0' $> False) <|> (char '1' $> True)

str :: Parser T.Text
str = takeWhile (\c -> c /= '\n' && c /= '\r') <&> TE.decodeUtf8

time :: Parser B.Time
time = signed decimal <&> B.Milliseconds

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
      ctor
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

    ctor
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
    ctor
      <$> kvDef "Bookmarks" (time `sepBy` char ',') []
      <*> kv "DistanceSpacing" double
      <*> kv "BeatDivisor" decimal
      <*> kv "GridSize" decimal
      <*> kv "TimelineZoom" double
  where
    kvP = parseKV colonspace
    kv k v = toPermutation $ kvP k v
    kvDef k v def = toPermutationWithDefault def $ kvP k v

    ctor
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
    ctor
      <$> kv "TitleUnicode" str
      <*> kv "Title" str
      <*> kv "ArtistUnicode" str
      <*> kv "Artist" str
      <*> kv "Creator" str
      <*> kv "Version" str
      <*> kv "Source" str
      <*> kv "Tags" (str `sepBy` char ' ')
      <*> kv "BeatmapID" decimal
      <*> kv "BeatmapSetID" decimal
  where
    kvP = parseKV colon
    kv k v = toPermutation $ kvP k v

    ctor
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
beatmapDifficulty =
  return
    B.BeatmapDifficulty
      { B.hpDrainRate = 0,
        B.circleSize = 0,
        B.overallDifficulty = 0,
        B.approachRate = 0,
        B.sliderBaseVelocity = 0,
        B.sliderTicksPerBeat = 0
      }

beatmapEvents :: Parser [B.BeatmapEvent]
beatmapEvents = return []

beatmapTimings :: Parser [B.TimingPoint]
beatmapTimings = return []

beatmapColors :: Parser B.BeatmapColors
beatmapColors =
  return
    B.BeatmapColors
      { B.combos = Map.empty,
        B.sliderTrackOverride = Nothing,
        B.sliderBorder = Nothing
      }

beatmapObjects :: Parser [B.HitObject]
beatmapObjects = return []
