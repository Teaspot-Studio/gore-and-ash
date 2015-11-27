module Game(
    mainWire
  , Player(..)
  , Camera(..)
  , Game(..)
  ) where

import Data.Functor.Identity
import Game.GoreAndAsh
import Control.Wire 
import Control.DeepSeq
import GHC.Generics (Generic)
import Linear

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

mainWire :: GameWire Identity a Game
mainWire = Game
  <$> playerWire initialPlayer
  <*> cameraWire initialCamera 
  where 
    initialCamera = Camera 0 0 0
    initialPlayer = Player 0 (V3 1 0 0) 0

cameraWire :: Camera -> GameWire Identity a Camera 
cameraWire initialCamera = loop $ proc (_, c_) -> do 
  c <- delay initialCamera -< c_
  forceNF -< (c, c)

playerWire :: Player -> GameWire Identity a Player 
playerWire initialPlayer = loop $ proc (_, p_) -> do 
  p <- delay initialPlayer -< p_ 
  forceNF -< (p, p)