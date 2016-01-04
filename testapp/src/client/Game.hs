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

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network

import Game.Camera 
import Game.Core
import Game.Player

data Game = Game {
  gamePlayer :: !Player 
, gameCamera :: !Camera  
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a (Maybe Game)
mainWire = waitConnection
  where 
    initialCamera i = Camera i 0 0 (-0.1)
    initialPlayer peer i = Player {
        playerId = i 
      , playerPos = 0
      , playerColor = V3 1 0 0
      , playerRot = 0
      , playerSpeed = 0.5
      , playerPeer = peer
      }

    waitConnection = proc a -> do 
      e <- mapE head . peersConnected -< ()
      traceEvent (const "Connected to server") -< e
      rSwitch (pure Nothing) -< (a, untilDisconnected <$> e)

    untilDisconnected peer = proc a -> do 
      e <- peerDisconnected peer -< ()
      traceEvent (const "Disconnected from server") -< e
      rSwitch (playGame peer) -< (a, const disconnected <$> e)

    playGame peer = Just <$> (Game
      <$> runActor' (playerWire $ initialPlayer peer)
      <*> runActor' (cameraWire initialCamera))

    disconnected = pure Nothing