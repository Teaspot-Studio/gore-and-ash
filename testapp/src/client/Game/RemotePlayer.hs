module Game.RemotePlayer(
    RemotePlayer(..)
  , remotePlayerActor
  ) where

import Control.Wire
import Linear

import Game.Camera
import Game.Core
import Game.Data()
import Game.Player.Data
import Game.Shared 
import Graphics.Square

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network 
import Game.GoreAndAsh.Sync

import Game.RemotePlayer.Data

-- | Actor for updating local state of remote player on server
remotePlayerActor :: Peer -> PlayerId -> AppActor PlayerId Camera RemotePlayer
remotePlayerActor peer pid = do 
  peerSendIndexedM peer (ChannelID 0) globalGameId ReliableMessage $ PlayerRequestData $ toCounter pid
  makeFixedActor pid $ stateWire initPlayer $ proc (c, p) -> do 
    p2 <- peerProcessIndexed peer (ChannelID 0) pid netProcess -< p
    liftGameMonad4 renderSquare -< (remotePlayerSize p2, remotePlayerPos p2, remotePlayerCol p2, c)
    forceNF -< p2
    where
    initPlayer = RemotePlayer {
        remotePlayerId = pid 
      , remotePlayerPos = 0 
      , remotePlayerRot = 0
      , remotePlayerCol = 0
      , remotePlayerSpeed = 0
      , remotePlayerSize = 1
      }

    netProcess :: RemotePlayer -> PlayerNetMessage -> RemotePlayer 
    netProcess p msg = case msg of 
      NetMsgPlayerPos x y -> p { remotePlayerPos = V2 x y }
      NetMsgPlayerRot r -> p { remotePlayerRot = r }
      NetMsgPlayerColor r g b -> p { remotePlayerCol = V3 r g b }
      NetMsgPlayerSpeed v -> p { remotePlayerSpeed = v }
      NetMsgPlayerSize s -> p { remotePlayerSize = s }
      NetMsgPlayerRequest -> p
      _ -> p