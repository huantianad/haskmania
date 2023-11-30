{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad

import Data.Time.Clock.System (getSystemTime, SystemTime (MkSystemTime, systemSeconds, systemNanoseconds))

import Sound.ProteaAudio

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import Brick.BChan
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (use, (.=))

data MyState = MyState {
    _currentMs :: Int
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
        _ -> return ()
appEvent (AppEvent (Tick x)) = currentMs .= x
appEvent _ = return ()

initialState :: MyState
initialState = MyState {
    _currentMs = 0
}

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

theApp :: M.App MyState Tick ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

-- Perhaps split this into two bits in the future, one for audio system,
-- one for sample. How important is sampleDestroy-ing the sample tho
withSample :: (Sample -> IO ()) -> IO ()
withSample = bracket aquire release where
    aquire = do
        result <- initAudio 64 48000 1024
        unless result $ fail "Failed to initialize the audio system."
        sampleFromFile "audio/test.ogg" 1.0

    release sample = do
        result <- sampleDestroy sample
        unless result $ fail "Failed to destroy sample."
        finishAudio

newtype Tick = Tick Int

msDiff :: SystemTime -> SystemTime -> Int
msDiff
    (MkSystemTime {systemSeconds = as, systemNanoseconds = an}) 
    (MkSystemTime {systemSeconds = bs, systemNanoseconds = bn}) 
        = (fromIntegral as - fromIntegral bs) * 1000 
        + (fromIntegral an - fromIntegral bn) `div` 1000000

soundThread :: BChan Tick -> IO ()
soundThread bChan = withSample $ \sample -> do
    startTime <- getSystemTime
    _ <- soundPlay sample 1 1 0 1
    forever $ do
        newTime <- getSystemTime
        writeBChan bChan $ Tick $ msDiff newTime startTime
        threadDelay 100000
        
main :: IO ()
main = do
    bChan <- newBChan 10
    void $ forkIO $ soundThread bChan

    void $ M.customMainWithDefaultVty (Just bChan) theApp initialState
