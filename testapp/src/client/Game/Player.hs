module Game.Player(
    module ReExport
  , playerActor
  , remotePlayerActor
  ) where

import Control.Wire
import Control.Wire.Unsafe.Event
import Linear
import Prelude hiding (id, (.))

import Game.Core
import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL 
import Game.GoreAndAsh.Sync

import Game.Camera 
import Game.Player.Data as ReExport
import Graphics.Square

playerActor :: Peer -> PlayerId -> AppActor PlayerId Camera Player 
playerActor peer i = makeFixedActor i $ stateWire initialPlayer $ proc (c, p) -> do 
  processFire -< (c, p)
  liftGameMonad4 renderSquare -< (playerSize p, playerPos p, playerColor p, c)
  forceNF . clientSync playerSync peer i . controlPlayer -< p
  where
    initialPlayer = Player {
        playerId = i 
      , playerPos = 0
      , playerColor = V3 1 0 0
      , playerRot = 0
      , playerSpeed = 0.5
      , playerSize = 1
      }
      
    controlPlayer :: AppWire Player Player
    controlPlayer = 
        movePlayer (V2 (-1) 0) ScancodeRight
      . movePlayer (V2 1 0) ScancodeLeft
      . movePlayer (V2 0 1) ScancodeDown
      . movePlayer (V2 0 (-1)) ScancodeUp

    movePlayer :: V2 Double -> Scancode -> AppWire Player Player
    movePlayer dv k = proc p -> do 
      e <- keyPressing k -< ()
      dt <- deltaTime -< ()
      let newPlayer = p {
            playerPos = playerPos p + dv * v2 (dt * playerSpeed p)
          }
          v2 a = V2 a a
      returnA -< event p (const newPlayer) e

    processFire :: AppWire (Camera, Player) ()
    processFire = proc (c, p) -> do 
      e <- mouseClick ButtonLeft -< ()
      let wpos = cameraToWorld c <$> e
      let edir = (\v -> normalize $ v - playerPos p) <$> wpos 
      let emsg = NetMsgPlayerFire <$> edir
      peerSendIndexed peer (ChannelID 0) i ReliableMessage -< emsg
      returnA -< ()

    playerSync :: FullSync AppMonad PlayerId Player 
    playerSync = Player 
      <$> pure i 
      <*> clientSide peer 0 playerPos
      <*> clientSide peer 1 playerColor
      <*> clientSide peer 2 playerRot
      <*> clientSide peer 3 playerSpeed
      <*> clientSide peer 4 playerSize

-- | Actor for updating local state of remote player on server
remotePlayerActor :: Peer -> PlayerId -> AppActor PlayerId Camera Player
remotePlayerActor peer i = makeFixedActor i $ stateWire initPlayer $ proc (c, p) -> do 
    liftGameMonad4 renderSquare -< (playerSize p, playerPos p, playerColor p, c)
    forceNF . clientSync playerSync peer i -< p
    where
    initPlayer = Player {
        playerId = i 
      , playerPos = 0 
      , playerColor = 0
      , playerRot = 0
      , playerSpeed = 0
      , playerSize = 1
      }

    playerSync :: FullSync AppMonad PlayerId Player 
    playerSync = Player 
      <$> pure i 
      <*> serverSide 0 playerPos
      <*> serverSide 1 playerColor
      <*> serverSide 2 playerRot
      <*> serverSide 3 playerSpeed
      <*> serverSide 4 playerSize