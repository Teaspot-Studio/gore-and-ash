module Game.Player(
    module ReExport
  , playerActor
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
  
playerActor :: PlayerId -> Peer -> AppActor PlayerId Camera Player 
playerActor i peer = makeFixedActor i $ stateWire initialPlayer $ proc (c, p) -> do 
  p2 <- peerProcessIndexed peer (ChannelID 0) i netProcess -< p
  peerSendIndexed peer (ChannelID 0) i ReliableMessage . now -< NetMsgPlayerRequest
  liftGameMonad4 renderSquare -< (playerSize p2, playerPos p2, playerColor p2, c)
  forceNF . controlPlayer i -< p2
  where
    initialPlayer = Player {
        playerId = i 
      , playerPos = 0
      , playerColor = V3 1 0 0
      , playerRot = 0
      , playerSpeed = 0.5
      , playerPeer = peer
      , playerSize = 1
      }

    netProcess :: Player -> PlayerNetMessage -> Player 
    netProcess p msg = case msg of 
      NetMsgPlayerPos x y -> p { playerPos = V2 x y }
      NetMsgPlayerRot r -> p { playerRot = r }
      NetMsgPlayerColor r g b -> p { playerColor = V3 r g b }
      NetMsgPlayerSpeed v -> p { playerSpeed = v }
      NetMsgPlayerSize s -> p { playerSize = s }
      NetMsgPlayerRequest -> p 
      _ -> p 
      
    controlPlayer :: PlayerId -> AppWire Player Player
    controlPlayer pid = 
        movePlayer pid (V2 (-1) 0) ScancodeLeft 
      . movePlayer pid (V2 1 0) ScancodeRight
      . movePlayer pid (V2 0 1) ScancodeDown
      . movePlayer pid (V2 0 (-1)) ScancodeUp

    movePlayer :: PlayerId -> V2 Double -> Scancode -> AppWire Player Player
    movePlayer pid dv k = proc p -> do 
      e <- keyPressing k -< ()
      let newPlayer = p {
            playerPos = playerPos p + dv * V2 (playerSpeed p) (playerSpeed p)
          }
          posMsg = let V2 x y = playerPos newPlayer in NetMsgPlayerPos x y
      peerSendIndexed peer (ChannelID 0) pid UnreliableMessage -< const posMsg <$> e
      returnA -< event p (const newPlayer) e