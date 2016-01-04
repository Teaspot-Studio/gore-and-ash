module Game.RemotePlayer(
    RemotePlayer(..)
  , remotePlayerActor
  ) where

import Control.Wire
import Control.DeepSeq
import GHC.Generics 
import Linear

import Game.Core
import Game.Player 
import Game.Player.Shared

import Game.GoreAndAsh.Network 
import Game.GoreAndAsh.Sync

data RemotePlayer = RemotePlayer {
  remotePlayerId :: !PlayerId 
, remotePlayerPos :: !(V2 Double)
, remotePlayerRot :: !Double 
, remotePlayerCol :: !(V3 Double)
} deriving Generic

instance NFData RemotePlayer

-- | Actor for updating local state of remote player on server
remotePlayerActor :: Peer -> PlayerId -> AppActor PlayerId a RemotePlayer
remotePlayerActor peer pid = actorMaker $ proc (_, p) -> do 
  forceNF -< p
  where
    actorMaker = netStateActorFixed pid initPlayer process 
      peer 1 netProcess

    initPlayer = RemotePlayer {
        remotePlayerId = pid 
      , remotePlayerPos = 0 
      , remotePlayerRot = 0
      , remotePlayerCol = 0
      }

    process :: RemotePlayer -> PlayerMessage -> RemotePlayer 
    process p _ = p 

    netProcess :: ChannelID -> RemotePlayer -> PlayerNetMessage -> RemotePlayer 
    netProcess _ p msg = case msg of 
      NetMsgPlayerPos x y -> p { remotePlayerPos = V2 x y }
      NetMsgPlayerRot r -> p { remotePlayerRot = r }
      NetMsgPlayerColor r g b -> p { remotePlayerCol = V3 r g b }
      _ -> p