{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Brick
  ( Padding (Pad),
    Widget,
    attrName,
    bg,
    emptyWidget,
    fill,
    hBox,
    on,
    padAll,
    padBottom,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    withAttr,
    withDefAttr,
    (<+>),
    (<=>),
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
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Time.Clock.System (SystemTime (MkSystemTime, systemNanoseconds, systemSeconds), getSystemTime)
import GameRow (Orientation (Horizontal, Vertical), RgbColor, RowElement (Block), drawRow)
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (.=))
import Lens.Micro.TH (makeLenses)
import Sound.ProteaAudio qualified as PA

data MyState = MyState
  { _currentTime :: Double, -- in seconds
    _sound :: Maybe PA.Sound,
    _isPlaying :: Bool
  }

makeLenses ''MyState

bpm :: Int
bpm = 85

rowOrientation :: Orientation
rowOrientation = Vertical

rowWidth :: Int
rowWidth = 3

drawUI :: MyState -> [Widget ()]
drawUI d =
  [ withDefAttr (attrName "background") $ currentBeat' <=> isPlaying',
    withDefAttr (attrName "background") $
      center $
        combine $
          str " " : do
            color <- [(255, 255, 0), (0, 255, 255), (255, 0, 255), (0, 255, 0)]
            let row = drawRow' color [Block 10 1 (255, 255, 255), Block 12 1 (255, 255, 255), Block 16 1 (255, 255, 255), Block 22 2 (255, 255, 255), Block 25 1 (255, 0, 0), Block 26 1 (0, 0, 255)]
            replicate rowWidth row ++ [str " "],
    withDefAttr (attrName "background") (fill ' ')
  ]
  where
    currentBeat = floor (d ^. currentTime / 60 * fromIntegral bpm) :: Int
    currentBeat' = withAttr D.dialogAttr $ str $ show currentBeat
    isPlaying' = withAttr D.buttonAttr $ str $ if _isPlaying d then "playing" else "paused"
    scroll = d ^. currentTime * 100

    combine = case rowOrientation of
      Horizontal -> vBox
      Vertical -> hBox
    center = case rowOrientation of
      Horizontal -> C.vCenter
      Vertical -> C.hCenter
    getSize = case rowOrientation of
      Horizontal -> T.availWidthL
      Vertical -> T.availHeightL
    drawRow' :: RgbColor -> [RowElement] -> Widget ()
    drawRow' color elements = T.Widget T.Fixed T.Fixed $ do
      context <- T.getContext
      T.render $ drawRow rowOrientation (context ^. getSize) (realToFrac scroll) color elements

appEvent :: BrickEvent () Tick -> T.EventM () MyState ()
appEvent (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> M.halt
    V.EvKey _ _ -> do
      s <- use sound
      p <- use isPlaying

      result <- liftIO $ PA.soundUpdate (fromJust s) p 1 1 0 1
      unless result $ liftIO $ fail "Failed to toggle sound state"
      isPlaying .= not p
    _ -> return ()
appEvent (AppEvent (Tick s x)) = currentTime .= x >> sound .= Just s
appEvent _ = return ()

initialState :: MyState
initialState =
  MyState
    { _currentTime = 0,
      _sound = Nothing,
      _isPlaying = True
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
withSample :: (PA.Sample -> IO ()) -> IO ()
withSample = bracket aquire release
  where
    aquire = do
      result <- PA.initAudio 64 48000 1024
      unless result $ fail "Failed to initialize the audio system."
      PA.sampleFromFile "audio/pigstep.mp3" 1.0

    release sample = do
      result <- PA.sampleDestroy sample
      unless result $ fail "Failed to destroy sample."
      PA.finishAudio

data Tick = Tick PA.Sound Double

toSeconds :: SystemTime -> Double
toSeconds (MkSystemTime {systemSeconds = s, systemNanoseconds = n}) = fromIntegral s + fromIntegral n * 1_000_000

soundThread :: BChan Tick -> IO ()
soundThread bChan = withSample $ \sample -> do
  s <- PA.soundPlay sample 1 1 0 1
  forever $ do
    newTime <- PA.soundPos s
    writeBChan bChan $ Tick s newTime
    threadDelay 10_000

main :: IO ()
main = do
  bChan <- newBChan 10
  void $ forkIO $ soundThread bChan

  void $ M.customMainWithDefaultVty (Just bChan) theApp initialState
