{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Brick
  ( Widget,
    attrName,
    bg,
    fill,
    hBox,
    on,
    str,
    vBox,
    withAttr,
    withDefAttr,
  )
import Brick.AttrMap qualified as A
import Brick.BChan
import Brick.Main qualified as M
import Brick.Types (BrickEvent (..))
import Brick.Types qualified as T
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core ((<=>))
import Brick.Widgets.Dialog qualified as D
import Conduit (findC, foldC)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, unless, void, when)
import Control.Monad.State (MonadState)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.List (genericLength)
import Data.Map qualified as DM
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (clamp)
import Data.Text qualified as Text
import GHC.List (foldl')
import Graphics.Vty qualified as V
import HaskMania.Color (applyAlpha)
import HaskMania.Data.Beatmap qualified as BM
import HaskMania.Data.Parser (openBeatmapSet, readFileFromBeatmapSet)
import HaskMania.Feedback (Feedback (Notice), drawFeedback, feedbackVisible)
import HaskMania.GameRow (Orientation (Horizontal, Vertical), RgbColor, RowElement (Block), drawRow)
import HaskMania.PauseScreen (pauseScreen)
import HaskMania.Settings qualified as SG
import HaskMania.SoundW qualified as SW
import HaskMania.TimeKeeper (TimeKeeper, getTime, initTimeKeeper, updateTime)
import Lens.Micro.Platform (at, ix, makeLenses, use, zoom, (%=), (%~), (&), (+=), (.=), (^.))
import Options.Applicative qualified as O
import Sound.ProteaAudio qualified as PA

data Judgement = Immaculate | GoodEnough | Whatever | Bleh deriving (Show, Eq, Enum, Bounded)

instance Ord Judgement where
  compare a b = compare (relativeRank a) (relativeRank b)
    where
      relativeRank :: Judgement -> Int
      relativeRank Immaculate = 1
      relativeRank GoodEnough = 2
      relativeRank Whatever = 3
      relativeRank Bleh = 4

judgementToWindow :: DM.Map Judgement Double
judgementToWindow = DM.fromList [(Immaculate, 0.020), (GoodEnough, 0.40), (Whatever, 0.120), (Bleh, 0.250)]

data ScoreKeeper = ScoreKeeper
  { _score :: Int,
    _currentCombo :: Int,
    _highestCombo :: Int,
    _previousHitJudgement :: Judgement,
    _hitOffsets :: [Int],
    _judgementsMap :: DM.Map Judgement Int
  }
  deriving (Show)

makeLenses ''ScoreKeeper

data MyState = MyState
  { _currentTime :: Double, -- in seconds
    _settings :: SG.MySettings,
    _sound :: SW.SoundW,
    _timeKeeper :: TimeKeeper,
    _notes :: [[Double]],
    _scoreKeeper :: ScoreKeeper,
    _feedback :: [Feedback]
  }

makeLenses ''MyState

scrollSpeed :: Int
scrollSpeed = 60

rowOrientation :: Orientation
rowOrientation = Vertical

rowPadding :: Int
rowPadding = 3

inputAudioOffset :: Double
inputAudioOffset = 0.220

posWithInputAudioOffset :: (MonadState MyState m) => m Double
posWithInputAudioOffset = subtract inputAudioOffset <$> use currentTime

visualOffset :: Double
visualOffset = 0.250

posWithVisualOffset :: MyState -> Double
posWithVisualOffset = subtract visualOffset <$> _currentTime

average :: (Real a1, Fractional a2) => [a1] -> a2
average xs = realToFrac (sum xs) / genericLength xs

noteCount :: MyState -> Int
noteCount = sum . DM.elems . _judgementsMap . _scoreKeeper

drawUI :: MyState -> [Widget ()]
drawUI d
  | all null (d ^. notes) =
      [ withDefAttr
          (attrName "background")
          ( withAttr D.buttonAttr $
              str $
                ("Score: " ++ show (d ^. (scoreKeeper . score)))
                  ++ "/"
                  ++ show (noteCount d * 3)
          )
          <=> withDefAttr (attrName "background") (withAttr D.buttonAttr $ str ("Combo: " ++ show (d ^. (scoreKeeper . highestCombo)) ++ "/" ++ show (noteCount d)))
          <=> withDefAttr (attrName "background") (withAttr D.buttonAttr $ str $ "Hits: " ++ show (d ^. (scoreKeeper . judgementsMap)))
      ]
  | SW.soundPaused (d ^. sound) =
      [ withDefAttr (attrName "background") (withAttr D.buttonAttr pauseScreen)
      ]
  | otherwise =
      fmap (centerLayer . drawFeedback (d ^. currentTime)) (d ^. feedback)
        ++ [
             -- withDefAttr (attrName "background") (withAttr D.buttonAttr $ str $ show $ map (!! 0) (d ^. notes))
             withDefAttr (attrName "background") (withAttr D.buttonAttr $ str $ "Combo: " ++ show (d ^. scoreKeeper))
               <=> withDefAttr (attrName "background") (withAttr D.buttonAttr $ str $ "Average: " ++ show (average (d ^. (scoreKeeper . hitOffsets)) :: Double)),
             withDefAttr (attrName "background") $ center $ combine stuff,
             withDefAttr (attrName "background") (fill ' ')
           ]
  where
    combine = case rowOrientation of
      Horizontal -> vBox
      Vertical -> hBox
    center = case rowOrientation of
      Horizontal -> C.vCenterLayer
      Vertical -> C.hCenterLayer
    getSize = case rowOrientation of
      Horizontal -> T.availWidthL
      Vertical -> T.availHeightL

    drawRow' :: RgbColor -> [Double] -> Maybe Char -> Widget ()
    drawRow' color noteTimes char = T.Widget T.Fixed T.Fixed $ do
      context <- T.getContext
      let elements = map (Block (applyAlpha 1 color) 1 . (* fromIntegral scrollSpeed)) noteTimes

      T.render $ drawRow rowOrientation (context ^. getSize) (posWithVisualOffset d * fromIntegral scrollSpeed) color elements char

    stuff =
      str " " : do
        let s = d ^. settings
        ((V.KChar char, color), blocks) <-
          zip
            [ (s ^. SG.columnOneKey, (255, 255, 0)),
              (s ^. SG.columnTwoKey, (0, 255, 255)),
              (s ^. SG.columnThreeKey, (255, 0, 255)),
              (s ^. SG.columnFourKey, (0, 255, 0))
            ]
            (d ^. notes)
        let row = drawRow' color blocks
        replicate rowPadding (row Nothing) ++ [row (Just char)] ++ replicate rowPadding (row Nothing) ++ [str " "]

appEvent :: BrickEvent () Tick -> T.EventM () MyState ()
appEvent (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> zoom sound SW.togglePause
    V.EvKey V.KEnter [] -> M.halt
    V.EvKey key [] -> do
      s <- use settings
      case key of
        -- placeholders
        k | k == (s ^. SG.volumeUpKey) -> zoom sound (SW.volumeAdjust SW.Increase)
        k | k == (s ^. SG.volumeDownKey) -> zoom sound (SW.volumeAdjust SW.Decrease)
        k | k == (s ^. SG.columnOneKey) -> handleLaneInput 0
        k | k == (s ^. SG.columnTwoKey) -> handleLaneInput 1
        k | k == (s ^. SG.columnThreeKey) -> handleLaneInput 2
        k | k == (s ^. SG.columnFourKey) -> handleLaneInput 3
        -- how to change keybindings, hopefully the new key will be inputted
        V.KChar '+' -> zoom settings (SG.volumeUpKey .= V.KChar '>')
        _ -> return ()
    V.EvKey _ _ -> return ()
    _ -> return ()
appEvent (AppEvent Tick) = do
  s <- use sound
  zoom timeKeeper $ updateTime s
  zoom timeKeeper (getTime s) >>= (currentTime .=)

  removeOldNotesM
appEvent _ = return ()

removeOldNotesM :: T.EventM () MyState ()
removeOldNotesM = do
  pos <- posWithInputAudioOffset
  n <- use notes
  let updatedNotes = map (removeOldNotes pos) n
  notes .= map fst updatedNotes
  when (any snd updatedNotes) $ zoom scoreKeeper $ updateScore Bleh

updateScore :: Judgement -> T.EventM () ScoreKeeper ()
updateScore judgement = do
  previousHitJudgement .= judgement

  case judgement of
    Bleh -> currentCombo .= 0
    _ -> currentCombo += 1

  updateHighestCombo <- (>) <$> use currentCombo <*> use highestCombo
  when updateHighestCombo $ use currentCombo >>= (highestCombo .=)

  score += case judgement of
    Immaculate -> 3
    GoodEnough -> 2
    Whatever -> 1
    Bleh -> 0

  judgementsMap . at judgement %= Just . (+ 1) . fromJust

createFeedback :: Judgement -> Double -> Feedback
createFeedback Bleh = Notice (248, 113, 113) "BLEH"
createFeedback Whatever = Notice (250, 204, 21) "WHATEVER"
createFeedback GoodEnough = Notice (163, 230, 53) "GOOD ENOUGH"
createFeedback Immaculate = Notice (232, 121, 249) "IMMACULATE"

-- | Handles the input for a specific lane in the game.
--   Removes the next note from the lane's note list if the
--   current time is within a certain range around the note.
--   The range is defined by an offset of +/- 0.250 seconds.
handleLaneInput ::
  -- | The index of the lane
  Int ->
  -- | The event monad action
  T.EventM () MyState ()
handleLaneInput index = do
  n <- use notes
  pos <- posWithInputAudioOffset
  t <- use currentTime
  fbs <- use feedback
  case n !! index of
    [] -> return ()
    nextNote : rest -> case checkNote pos nextNote of
      Nothing -> return ()
      Just judgement -> do
        notes . ix index .= rest
        (scoreKeeper . hitOffsets) %= (floor ((pos - nextNote) * 1000) :)
        zoom scoreKeeper $ updateScore judgement
        feedback .= createFeedback judgement t : filter (feedbackVisible t) fbs

-- Given an input position and a note's position, returns what
-- judgement window the input falls under, or Nothing if the input
-- is outside of the hit window entirely
checkNote :: Double -> Double -> Maybe Judgement
checkNote pos note = DM.foldlWithKey func Nothing judgementToWindow
  where
    func :: Maybe Judgement -> Judgement -> Double -> Maybe Judgement
    func prevJudge@(Just _) _ _ = prevJudge
    func Nothing thisJudge thisWindow
      | note - thisWindow < pos && pos < note + thisWindow = Just thisJudge
      | otherwise = Nothing

-- | Removes old notes from a list based on a given position.
--   An old note is defined as a note that is less than (pos - 0.250).
--   Returns a tuple containing the new list of notes and a boolean
--   indicating whether any notes were removed. Expects input to be sorted!
--
--   Examples:
--
--   >>> removeOldNotes 0.5 [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
--   ([0.3,0.4,0.5,0.6],True)
--
--   >>> removeOldNotes 1.0 [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
--   ([],True)
removeOldNotes :: Double -> [Double] -> ([Double], Bool)
removeOldNotes _ [] = ([], False)
removeOldNotes pos (x : xs) = if x < (pos - 0.250) then (fst (removeOldNotes pos xs), True) else (x : xs, False)

initialState :: SW.SoundW -> TimeKeeper -> SG.MySettings -> [[Double]] -> MyState
initialState s tk ds n =
  MyState
    { _currentTime = 0,
      _settings = ds,
      _sound = s,
      _timeKeeper = tk,
      _notes = n,
      _scoreKeeper =
        ScoreKeeper
          { _score = 0,
            _currentCombo = 0,
            _highestCombo = 0,
            _previousHitJudgement = Immaculate,
            _hitOffsets = [],
            _judgementsMap = DM.fromList $ map (,0) [minBound .. maxBound]
          },
      _feedback = [Notice (255, 255, 255) "BEGIN" 0]
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow),
      (attrName "background", V.black `on` V.black)
    ]

theApp :: M.App MyState Tick ()
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

-- Perhaps split this into two bits in the future, one for audio system,
-- one for sample. How important is sampleDestroy-ing the sample tho
withSample :: IO PA.Sample -> (PA.Sample -> IO ()) -> IO ()
withSample sample = bracket acquire release
  where
    -- \| This function initializes the audio system and plays a sample from a file.
    -- It takes the file path as an argument and returns a result indicating whether
    -- the audio system was successfully initialized.
    acquire :: IO PA.Sample
    acquire = do
      result <- PA.initAudio 64 48000 256
      unless result $ fail "Failed to initialize the audio system."
      sample

    -- \| Releases the given audio sample.
    --   It destroys the sample and finishes the audio.
    release :: PA.Sample -> IO ()
    release sample' = do
      result <- PA.sampleDestroy sample'
      unless result $ fail "Failed to destroy sample."
      PA.finishAudio

data Tick = Tick

soundThread :: BChan Tick -> IO ()
soundThread bChan = forever $ do
  writeBChan bChan Tick
  threadDelay 10_000

data Args = Args
  { beatmapSetPath :: FilePath,
    version :: String
  }

loadBeatmap :: Args -> IO (Maybe BM.Beatmap)
loadBeatmap (Args {beatmapSetPath, version}) =
  runConduitRes $
    openBeatmapSet beatmapSetPath
      .| findC (fuzzyMatch (Text.pack version) . BM.version . BM.metadata)
  where
    fuzzyMatch query = Text.isInfixOf (Text.toCaseFold query) . Text.toCaseFold

loadBeatmapAudio :: Args -> BM.Beatmap -> IO ByteString
loadBeatmapAudio (Args {beatmapSetPath}) bm =
  runConduitRes $
    readFileFromBeatmapSet beatmapSetPath ((BM.audioFilename . BM.info) bm)
      .| foldC

loadBeatmapAndAudio :: Args -> IO (BM.Beatmap, ByteString)
loadBeatmapAndAudio args = do
  bm <- fromMaybe (error "failed to find beatmap") <$> loadBeatmap args
  sampleBytes <- loadBeatmapAudio args bm
  return (bm, sampleBytes)

-- Converts an osu!mania beatmap's hitobject data into a simplified representation.
-- The resulting 2D array represents each lane as a row,
-- where the values in the row are positions of notes in seconds.
beatmapToNotes :: BM.Beatmap -> [[Double]]
beatmapToNotes beatmap =
  let hitObjects = BM.objects beatmap
      colCount = 4

      posToColIdx :: BM.Position -> Int
      posToColIdx (x, _) = clamp (0, colCount - 1) $ x * colCount `div` 512

      hitObjToNote :: BM.HitObject -> Maybe (Int, Double)
      hitObjToNote BM.HitObject {BM.position = pos, BM.kind = kind, BM.time = BM.Milliseconds time} = case kind of
        BM.Circle _ -> Just (posToColIdx pos, fromIntegral time / 1000)
        BM.Hold _ -> Just (posToColIdx pos, fromIntegral time / 1000)
        _ -> Nothing

      foldFunc :: [[Double]] -> BM.HitObject -> [[Double]]
      foldFunc base new = case hitObjToNote new of
        Nothing -> base
        Just (col, time) -> base & ix col %~ (time :)
   in map reverse $ foldl' foldFunc (replicate colCount []) hitObjects

initApp :: Args -> IO ()
initApp args = do
  (bm, sampleBytes) <- loadBeatmapAndAudio args

  withSample (PA.sampleFromMemory sampleBytes 1.0) $ \sample -> do
    s <- SW.soundPlay sample
    tk <- initTimeKeeper s
    let ds = SG.defaultSettings
    let n = beatmapToNotes bm

    bChan <- newBChan 10
    void $ forkIO $ soundThread bChan

    void $ M.customMainWithDefaultVty (Just bChan) theApp $ initialState s tk ds n

argsP :: O.Parser Args
argsP =
  Args
    <$> O.argument
      O.str
      ( O.metavar "FILE"
          <> O.help "beatmap set to load"
      )
    <*> O.argument
      O.str
      ( O.metavar "VERSION"
          <> O.help "beatmap version to load from set"
      )

cli :: O.ParserInfo Args
cli =
  O.info
    (argsP O.<**> O.helper)
    ( O.fullDesc
        <> O.header "HaskMania"
        <> O.progDesc "a VSRG implemented on the command line"
    )

main :: IO ()
main = do
  args <- O.execParser cli
  initApp args
