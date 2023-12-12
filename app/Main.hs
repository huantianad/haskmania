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
import Control.Monad
import Graphics.Vty qualified as V
import HaskMania.GameRow (Orientation (Horizontal, Vertical), RgbColor, RowElement (Block), drawRow)
import HaskMania.PauseScreen (pauseScreen)
import HaskMania.Settings qualified as SG
import HaskMania.SoundW qualified as SW
import HaskMania.TimeKeeper (TimeKeeper, getTime, initTimeKeeper, updateTime)
import Lens.Micro.Platform (makeLenses, use, zoom, (.=), (^.))
import Sound.ProteaAudio qualified as PA

data MyState = MyState
  { _currentTime :: Double, -- in seconds
    _settings :: SG.MySettings,
    _sound :: SW.SoundW,
    _timeKeeper :: TimeKeeper
  }

makeLenses ''MyState

bpm :: Int
bpm = 96

rowOrientation :: Orientation
rowOrientation = Vertical

rowPadding :: Int
rowPadding = 2

drawUI :: MyState -> [Widget ()]
drawUI d
  | not $ SW.soundPaused (d ^. sound) =
      [ withDefAttr (attrName "background") $
          center $
            combine $
              str " " : do
                (char, color, blocks) <-
                  [ ('h', (255, 255, 0), do i <- [0 ..]; [Block (255, 255, 0, 0.8) 1 (i * 20), Block (255, 255, 255, 0.2) 1 (i * 20 + 1)]),
                    ('j', (0, 255, 255), map (\i -> Block (0, 255, 255, 1) 5.5 (i * 20)) [0 ..]),
                    ('k', (255, 0, 255), do i <- [0 ..]; [Block (255, 0, 255, 1) 1 (i * 20), Block (255, 0, 255, 0.7) 2 (i * 20 + 1)]),
                    ('l', (0, 255, 0), do i <- [0 ..]; [Block (0, 255, 0, 1) 1 (i * 20)])
                    ]
                let row = drawRow' color blocks
                replicate rowPadding (row Nothing) ++ [row (Just char)] ++ replicate rowPadding (row Nothing) ++ [str " "],
        withDefAttr (attrName "background") (fill ' ')
      ]
  | otherwise = [withDefAttr (attrName "background") (withAttr D.buttonAttr pauseScreen)]
  where
    currentBeat = d ^. currentTime / 60 * fromIntegral bpm
    -- 40 units per beat
    scroll = currentBeat * 40

    combine = case rowOrientation of
      Horizontal -> vBox
      Vertical -> hBox
    center = case rowOrientation of
      Horizontal -> C.vCenterLayer
      Vertical -> C.hCenterLayer
    getSize = case rowOrientation of
      Horizontal -> T.availWidthL
      Vertical -> T.availHeightL

    drawRow' :: RgbColor -> [RowElement] -> Maybe Char -> Widget ()
    drawRow' color elements char = T.Widget T.Fixed T.Fixed $ do
      context <- T.getContext
      T.render $ drawRow rowOrientation (context ^. getSize) (realToFrac scroll) color elements char

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
        V.KChar '+' -> zoom settings (SG.changeKey SG.volumeUpKey (V.KChar '>') s)
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
      _timeKeeper = tk
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
    aquire = do
      result <- PA.initAudio 64 48000 256
      unless result $ fail "Failed to initialize the audio system."
      PA.sampleFromFile filePath 1.0

    release sample = do
      result <- PA.sampleDestroy sample
      unless result $ fail "Failed to destroy sample."
      PA.finishAudio

data Tick = Tick

soundThread :: BChan Tick -> IO ()
soundThread bChan = do
  forever $ do
    writeBChan bChan Tick
    threadDelay 10_000

main :: IO ()
main = withSample "audio/test.ogg" $ \sample -> do
  s <- SW.soundPlay sample
  tk <- initTimeKeeper s
  let ds = SG.defaultSettings

  bChan <- newBChan 10
  void $ forkIO $ soundThread bChan

  let state = initialState s tk ds
  void $ M.customMainWithDefaultVty (Just bChan) theApp state
