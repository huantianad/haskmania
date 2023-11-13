module Main (main) where

import Control.Monad
import Control.Monad.IO.Class

import Sound.ProteaAudio.SDL

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
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (use, (.=))

data Choice = Red | Blue | Green
    deriving Show

data Name =
    RedButton
    | BlueButton
    | GreenButton
    deriving (Show, Eq, Ord)

data MyState = MyState {
    _dialog :: D.Dialog Choice Name,
    _sound :: Sound,
    _playing :: Bool
}
makeLenses ''MyState

drawUI :: MyState -> [Widget Name]
drawUI d = [ui]
    where
        ui = D.renderDialog (_dialog d) $ C.hCenter $ padAll 1 $ str "This is the dialog body."

appEvent :: BrickEvent Name e -> T.EventM Name MyState ()
appEvent (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt
        V.EvKey V.KEnter [] -> M.halt
        _ -> do
            T.zoom dialog $ D.handleDialogEvent ev

            soundState <- use sound
            playingState <- use playing
            
            _ <- liftIO $ soundUpdate soundState (not playingState) 1 1 0 1
            playing .= not playingState
appEvent _ = return ()

initialState :: MyState
initialState = MyState { _dialog = D.dialog (Just $ str "Title") (Just (RedButton, choices)) 50, _playing = False }
    where
        choices = [ ("Red",   RedButton,   Red)
                  , ("Blue",  BlueButton,  Blue)
                  , ("Green", GreenButton, Green)
                  ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

myStartEvent :: T.EventM Name MyState ()
myStartEvent = do
    result <- liftIO $ initAudio 64 48000 1024
    liftIO $ unless result $ fail "Failed to initialize the audio system!!"

    sample <- liftIO $ sampleFromFile "audio/test.ogg" 1.0
    soundState <- liftIO $ soundPlay sample 1 1 0 1

    sound .= soundState

theApp :: M.App MyState e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = myStartEvent
          , M.appAttrMap = const theMap
          }


main :: IO ()
main = do
    d <- M.defaultMain theApp initialState
    putStrLn $ "You chose: " <> show (D.dialogSelection $ _dialog d)

    -- _ <- sampleDestroy sample
    finishAudio

