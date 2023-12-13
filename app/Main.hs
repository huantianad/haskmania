{-# LANGUAGE LambdaCase #-}

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
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Dialog qualified as D
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, unless, void)
import Graphics.Vty qualified as V
import HaskMania.Data.Beatmap qualified as BM
import HaskMania.GameRow (Orientation (Horizontal, Vertical), RgbColor, RowElement (Block), drawRow, mixAlpha)
import HaskMania.PauseScreen (pauseScreen)
import HaskMania.Settings qualified as SG
import HaskMania.SoundW qualified as SW
import HaskMania.TimeKeeper (TimeKeeper, getTime, initTimeKeeper, updateTime)
import Lens.Micro.Platform (makeLenses, use, zoom, (.=), (^.))
import Sound.ProteaAudio qualified as PA
import System.Environment (getArgs)
import System.Exit (exitFailure)

data MyState = MyState
  { _currentTime :: Double, -- in seconds
    _settings :: SG.MySettings,
    _sound :: SW.SoundW,
    _timeKeeper :: TimeKeeper,
    _notes :: [[Double]]
  }

makeLenses ''MyState

bpm :: Int
bpm = 96

scrollSpeed :: Int
scrollSpeed = 20

rowOrientation :: Orientation
rowOrientation = Vertical

rowPadding :: Int
rowPadding = 2

drawUI :: MyState -> [Widget ()]
drawUI d
  | SW.soundPaused (d ^. sound) =
      [ withDefAttr (attrName "background") (withAttr D.buttonAttr pauseScreen)
      ]
  | otherwise =
      [ withDefAttr (attrName "background") $ center $ combine stuff,
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
      let elements = map (Block (mixAlpha 1 color) 1 . (* fromIntegral scrollSpeed)) noteTimes

      T.render $ drawRow rowOrientation (context ^. getSize) (d ^. currentTime * fromIntegral scrollSpeed) color elements char

    stuff = do
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
      str " " : replicate rowPadding (row Nothing) ++ [row (Just char)] ++ replicate rowPadding (row Nothing) ++ [str " "]

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
        k | k == (s ^. SG.columnOneKey) -> return ()
        k | k == (s ^. SG.columnTwoKey) -> return ()
        k | k == (s ^. SG.columnThreeKey) -> return ()
        k | k == (s ^. SG.columnFourKey) -> return ()
        -- how to change keybindings, hopefully the new key will be inputted
        V.KChar '+' -> zoom settings (SG.volumeUpKey .= V.KChar '>')
        _ -> return ()
    V.EvKey _ _ -> return ()
    _ -> return ()
appEvent (AppEvent Tick) = do
  s <- use sound
  zoom timeKeeper $ updateTime s
  zoom timeKeeper (getTime s) >>= (currentTime .=)
appEvent _ = return ()

initialState :: SW.SoundW -> TimeKeeper -> SG.MySettings -> MyState
initialState s tk ds =
  MyState
    { _currentTime = 0,
      _settings = ds,
      _sound = s,
      _timeKeeper = tk,
      _notes = [[0, 4 ..], [1, 5 ..], [2, 6 ..], [3, 7 ..]]
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
withSample :: String -> (PA.Sample -> IO ()) -> IO ()
withSample filePath = bracket aquire release
  where
    -- \| This function initializes the audio system and plays a sample from a file.
    -- It takes the file path as an argument and returns a result indicating whether
    -- the audio system was successfully initialized.
    aquire :: IO PA.Sample
    aquire = do
      result <- PA.initAudio 64 48000 256
      unless result $ fail "Failed to initialize the audio system."
      PA.sampleFromFile filePath 1.0

    -- \| Releases the given audio sample.
    --   It destroys the sample and finishes the audio.
    release :: PA.Sample -> IO ()
    release sample = do
      result <- PA.sampleDestroy sample
      unless result $ fail "Failed to destroy sample."
      PA.finishAudio

data Tick = Tick

soundThread :: BChan Tick -> IO ()
soundThread bChan = forever $ do
  writeBChan bChan Tick
  threadDelay 10_000

data Args = Args
  { beatmapFile :: String,
    difficulty :: Float
  }

-- TODO: parse command line arguments
parseCommandLine :: [String] -> IO (Maybe Args)
parseCommandLine [] = return Nothing
parseCommandLine (beatmapFile : args) = return $ Just (Args beatmapFile $ read $ head args)

initBeatmap :: Args -> IO BM.Beatmap
initBeatmap (Args beatmapFile difficulty) = do
  -- beatmap <- parseBeatmap beatmapFile
  -- choose map with closest star rating to difficulty
  -- return beatmap
  undefined

initApp :: Args -> IO ()
initApp (Args beatmapFile _) = withSample "audio/test.ogg" $ \sample -> do
  s <- SW.soundPlay sample
  tk <- initTimeKeeper s
  let ds = SG.defaultSettings

  bChan <- newBChan 10
  void $ forkIO $ soundThread bChan

  let state = initialState s tk ds
  void $ M.customMainWithDefaultVty (Just bChan) theApp state

main :: IO ()
main = do
  myArgs <- getArgs >>= parseCommandLine
  case myArgs of
    Nothing -> do
      putStrLn "Usage: ./cse230-project <beatmap> <difficulty>"
      exitFailure
    Just args -> do
      -- beatmap <- initBeatmap args
      initApp args
