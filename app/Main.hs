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
import Data.Time.Clock.System (SystemTime (MkSystemTime, systemNanoseconds, systemSeconds), getSystemTime)
import Graphics.Vty qualified as V
import HaskMania.GameRow (Orientation (Horizontal, Vertical), RgbColor, RowElement (Block), drawRow)
import HaskMania.TimeKeeper (TimeKeeper, getTimeM, initTimeKeeper, updateTimeM)
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, zoom, (.=))
import Lens.Micro.TH (makeLenses)
import Sound.ProteaAudio qualified as PA

data MyState = MyState
  { _currentTime :: Double, -- in seconds
    _sound :: PA.Sound,
    _timeKeeper :: TimeKeeper
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
  [ withDefAttr (attrName "background") $ currentBeat' <=> a,
    withDefAttr (attrName "background") $
      center $
        combine $
          str " " : do
            (color, blocks) <-
              [ ((255, 255, 0), [Block (255, 255, 255) 1 10, Block (255, 255, 255) 1 12, Block (255, 255, 255) 1 16, Block (255, 255, 255) 2 22, Block (255, 0, 0) 1 25, Block (0, 0, 255) 1 26]),
                ((0, 255, 255), map (\i -> Block (0, 255, 255) 1 (i * 2)) [0 ..]),
                ((255, 0, 255), map (\i -> Block (255, 0, 255) 2 (i * 3)) [0 ..]),
                ((0, 255, 0), map (\i -> Block (0, 255, 0) 1 (i * 5)) [0 ..])
                ]
            let row = drawRow' color blocks
            replicate rowWidth row ++ [str " "],
    withDefAttr (attrName "background") (fill ' ')
  ]
  where
    currentBeat = d ^. currentTime / 60 * fromIntegral bpm
    currentBeat' = withAttr D.dialogAttr $ str $ show (floor currentBeat)
    a = withAttr D.dialogAttr $ str $ show $ d ^. timeKeeper
    -- Two units per beat
    scroll = currentBeat * 2

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
      isPaused <- liftIO $ PA.soundPaused s
      result <- liftIO $ PA.soundUpdate s (not isPaused) 1 1 0 1
      unless result $ liftIO $ fail "Failed to toggle sound state"
    _ -> return ()
appEvent (AppEvent (Tick _)) = do
  zoom timeKeeper updateTimeM
  zoom timeKeeper getTimeM >>= (currentTime .=)
appEvent _ = return ()

initialState :: PA.Sound -> TimeKeeper -> MyState
initialState s tk =
  MyState
    { _currentTime = 0,
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
      result <- PA.initAudio 64 48000 1024
      unless result $ fail "Failed to initialize the audio system."
      PA.sampleFromFile filePath 1.0

    release sample = do
      result <- PA.sampleDestroy sample
      unless result $ fail "Failed to destroy sample."
      PA.finishAudio

data Tick = Tick Double

msDiff :: SystemTime -> SystemTime -> Int
msDiff
  (MkSystemTime {systemSeconds = as, systemNanoseconds = an})
  (MkSystemTime {systemSeconds = bs, systemNanoseconds = bn}) =
    (fromIntegral as - fromIntegral bs) * 1000 + (fromIntegral an - fromIntegral bn) `div` 1000000

soundThread :: PA.Sound -> BChan Tick -> IO ()
soundThread s bChan = do
  startTime <- getSystemTime
  forever $ do
    -- If you swap with the below, it's a lot slower. So something about soundPos is slow
    -- newTime <- PA.soundPos s
    -- writeBChan bChan $ Tick newTime
    newTime <- getSystemTime
    writeBChan bChan $ Tick $ fromIntegral (msDiff newTime startTime) / 1000
    threadDelay 1_000

main :: IO ()
main = withSample "audio/pigstep.mp3" $ \sample -> do
  s <- PA.soundPlay sample 1 1 0 1
  tk <- initTimeKeeper s

  bChan <- newBChan 10
  void $ forkIO $ soundThread s bChan

  let state = initialState s tk
  void $ M.customMainWithDefaultVty (Just bChan) theApp state
