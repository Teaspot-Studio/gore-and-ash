{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Player(..)
  , Camera(..)
  , Game(..)
  , AppMonad
  ) where

import Control.DeepSeq
import Control.Wire
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Actor 

import Game.Camera 
import Game.Core
import Game.Player

data Game = Game {
  gamePlayer :: !Player 
, gameCamera :: !Camera  
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a Game
mainWire = Game
  <$> runActor' (playerWire initialPlayer)
  <*> runActor' (cameraWire initialCamera)
  where 
    initialCamera = Camera 0 0 (-1)
    initialPlayer = Player 0 (V3 1 0 0) 0