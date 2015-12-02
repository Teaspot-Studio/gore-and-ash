module Game(
    mainWire
  , Player(..)
  , Camera(..)
  , Game(..)
  ) where

import Prelude hiding (id, (.))
import Data.Functor.Identity
import Game.GoreAndAsh
import Control.Wire 
import Control.DeepSeq
import GHC.Generics (Generic)
import Linear

import Game.GoreAndAsh.Logging

type AppWire a b = GameWire (LoggingT () Identity) a b

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
    initialCamera = Camera 0 0 0
    initialPlayer = Player 0 (V3 1 0 0) 0

cameraWire :: Camera -> AppWire a Camera 
cameraWire initialCamera = loop $ proc (_, c_) -> do 
  c <- delay initialCamera -< c_
  forceNF -< (c, c)

playerWire :: Player -> AppWire a Player 
playerWire initialPlayer = loop $ proc (_, p_) -> do 
  p <- delay initialPlayer -< p_ 
  traceEventShow . now -< "Hi, logging!"
  forceNF -< (p, p)