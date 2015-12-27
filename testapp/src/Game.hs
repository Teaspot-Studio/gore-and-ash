{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Player(..)
  , Camera(..)
  , Game(..)
  , AppMonad
  ) where

import Control.DeepSeq
import Control.Monad.Trans.Class
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Text
import Game.GoreAndAsh
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.GLFW 
import Game.GoreAndAsh.Network

type Layer1 = NetworkT () Identity
type Layer1State = NetworkState ()
type Layer2 = GLFWInputT Layer1State Layer1
type Layer2State = GLFWState (NetworkState ())
type AppMonad = LoggingT Layer2State Layer2
type AppWire a b = GameWire AppMonad a b

instance MonadGLFWInput AppMonad where
  keyStatusM = lift . keyStatusM
  mouseButtonM = lift . mouseButtonM
  mousePosM = lift mousePosM
  mouseScrollM = lift mouseScrollM
  windowSizeM = lift windowSizeM
  setCurrentWindowM = lift . setCurrentWindowM 
  
data Player = Player {
  playerPos :: !(V2 Float)
, playerColor :: !(V3 Float) 
, playerRot :: !Float  
} deriving (Generic)

instance NFData Player 

data Camera = Camera {
  cameraPos :: !(V2 Float)
, cameraRot :: !Float 
, cameraZoom :: !Float
} deriving (Generic)

instance NFData Camera 

data Game = Game {
  gamePlayer :: !Player 
, gameCamera :: !Camera  
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a Game
mainWire = Game
  <$> playerWire initialPlayer
  <*> cameraWire initialCamera 
  where 
    initialCamera = Camera 0 0 (-1)
    initialPlayer = Player 0 (V3 1 0 0) 0

cameraWire :: Camera -> AppWire a Camera 
cameraWire initialCamera = loop $ proc (_, c_) -> do 
  c <- delay initialCamera -< c_
  c2 <- moveCamera (V2 0 (-0.1)) Key'W 
    . moveCamera (V2 0 0.1) Key'S 
    . moveCamera (V2 0.1 0) Key'A
    . moveCamera (V2 (-0.1) 0) Key'D 
    . zoomCamera 0.1 -< c
  forceNF -< (c2, c2)
  where 
    moveCamera :: V2 Float -> Key -> AppWire Camera Camera
    moveCamera dv k = proc c -> do 
      e <- keyPressing k -< ()
      let newCam = c {
            cameraPos = cameraPos c + dv 
          }
      returnA -< event c (const newCam) e

    zoomCamera :: Float -> AppWire Camera Camera 
    zoomCamera z = proc c -> do 
      e <- mouseScrollY -< ()
      let newCam k = c {
            cameraZoom = max (-3) $ min (-0.01) $ cameraZoom c + z * k
          }
      returnA -< event c (newCam . realToFrac) e 

playerWire :: Player -> AppWire a Player 
playerWire initialPlayer = loop $ proc (_, p_) -> do 
  p <- delay initialPlayer -< p_ 
  -- traceEvent (pack . show) . keyPressed Key'W -< ()
  traceEvent (pack . show) . mouseButtonPressed MouseButton'1 -< ()
  -- traceEvent (pack . show) . mousePositionChange -< ()
  traceEvent (pack . show) . windowSize -< ()
  traceEvent (pack . show) . mouseScroll -< ()
  forceNF -< (p, p)