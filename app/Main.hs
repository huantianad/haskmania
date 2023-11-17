{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (bracket)

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
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl (use, (.=))
import Data.Foldable (foldlM)

data Choice = Red | Blue | Green
    deriving Show

data Name =
    RedButton
    | BlueButton
    | GreenButton
    deriving (Show, Eq, Ord)

data MyState = MyState {
    _dialog :: D.Dialog Choice Name,
    _currentSound :: Sound,
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

            soundState <- use currentSound
            playingState <- use playing

            _ <- liftIO $ soundUpdate soundState (not playingState) 1 1 0 1
            playing .= not playingState
appEvent _ = return ()

initialState :: Sound -> MyState
initialState sound = MyState {
    _dialog = D.dialog (Just $ str "Title") (Just (RedButton, choices)) 50,
    _currentSound = sound,
    _playing = False
}
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

theApp :: M.App MyState e Name
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
        unless result $ fail "Failed to initialize the audio system!!"
        sampleFromFile "audio/test.ogg" 1.0

    release sample = do
        result <- sampleDestroy sample
        unless result $ fail "Failed to destroy sample."
        finishAudio

main :: IO ()
main = withSample $ \sample -> do
    sound <- soundPlay sample 1 1 0 1

    d <- M.defaultMain theApp $ initialState sound
    putStrLn $ "You chose: " <> show (D.dialogSelection $ _dialog d)

data Parser a = Parser (String -> Maybe (a, String))
instance Functor Parser where
instance Applicative Parser where
    pure x = Parser $ \s -> pure (x, s)
instance Monad Parser where
    return = pure

    (>>=) x y = Parser $ \s -> do
        (a, s') <- runParser x s
        runParser (y a) s'

runParser (Parser f) = f

oneChar :: Parser Char
oneChar = Parser $ \case
    [] -> Nothing
    x:xs -> Just (x, xs)

failP :: Parser a
failP = Parser (\_ -> Nothing)

char c = do
    x <- oneChar
    if x == c then pure c else failP

string :: String -> Parser String
string = foldlM (\b a -> (b ++ [a]) <$ char a) ""


many :: [Parser a] -> Parser [a]
many = foldlM (\b a -> (\val -> b ++ [val]) <$> a) []
