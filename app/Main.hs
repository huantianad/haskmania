{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Brick.AttrMap qualified as A
import Brick.BChan
import Brick.Main qualified as M
import Brick.Types
  ( BrickEvent (..),
    Widget,
  )
import Brick.Types qualified as T
import Brick.Util (bg, on)
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core
  ( padAll,
    str,
  )
import Brick.Widgets.Dialog qualified as D
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Time.Clock.System (SystemTime (MkSystemTime, systemNanoseconds, systemSeconds), getSystemTime)
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (.=))
import Lens.Micro.TH (makeLenses)
import Sound.ProteaAudio

data MyState = MyState
  { _currentMs :: Int,
    _sound :: Maybe Sound,
    _isPlaying :: Bool
  }

makeLenses ''MyState

drawUI :: MyState -> [Widget ()]
drawUI d = [ui]
  where
    ui = C.hCenter $ padAll 1 $ str $ show (d ^. currentMs)

appEvent :: BrickEvent () Tick -> T.EventM () MyState ()
appEvent (VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KEnter [] -> M.halt
    _ -> do
      s <- use sound
      p <- use isPlaying

      result <- liftIO $ soundUpdate (fromJust s) (not p) 1 1 0 1
      unless result $ liftIO $ fail "Failed to toggle sound state"
      isPlaying .= not p
appEvent (AppEvent (Tick s x)) = currentMs .= x >> sound .= Just s
appEvent _ = return ()

initialState :: MyState
initialState =
  MyState
    { _currentMs = 0,
      _sound = Nothing,
      _isPlaying = True
    }

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue),
      (D.buttonAttr, V.black `on` V.white),
      (D.buttonSelectedAttr, bg V.yellow)
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
withSample :: (Sample -> IO ()) -> IO ()
withSample = bracket aquire release
  where
    aquire = do
      result <- initAudio 64 48000 1024
      unless result $ fail "Failed to initialize the audio system."
      sampleFromFile "audio/test.ogg" 1.0

    release sample = do
      result <- sampleDestroy sample
      unless result $ fail "Failed to destroy sample."
      finishAudio

data Tick = Tick Sound Int

msDiff :: SystemTime -> SystemTime -> Int
msDiff
  (MkSystemTime {systemSeconds = as, systemNanoseconds = an})
  (MkSystemTime {systemSeconds = bs, systemNanoseconds = bn}) =
    (fromIntegral as - fromIntegral bs) * 1000 + (fromIntegral an - fromIntegral bn) `div` 1000000

soundThread :: BChan Tick -> IO ()
soundThread bChan = withSample $ \sample -> do
  startTime <- getSystemTime
  s <- soundPlay sample 1 1 0 1
  forever $ do
    newTime <- getSystemTime
    writeBChan bChan $ Tick s $ msDiff newTime startTime
    threadDelay 100000

main :: IO ()
main = do
  bChan <- newBChan 10
  void $ forkIO $ soundThread bChan

  void $ M.customMainWithDefaultVty (Just bChan) theApp initialState
