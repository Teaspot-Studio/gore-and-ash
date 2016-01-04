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
import Game.GoreAndAsh.Sync 

import Game.Camera 
import Game.Core
import Game.Player
import Game.Shared
import Game.RemotePlayer

data Game = Game {
  gameId :: !GameId
, gamePlayer :: !Player 
, gameCamera :: !Camera
, gameRemotePlayers :: ![RemotePlayer] 
, gameAddPlayers :: ![PlayerId]
, gameRemovePlayers :: ![PlayerId]
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a (Maybe Game)
mainWire = waitConnection
  where
    waitConnection = proc a -> do 
      e <- mapE head . peersConnected -< ()
      traceEvent (const "Connected to server") -< e
      rSwitch (pure Nothing) -< (a, untilDisconnected <$> e)

    untilDisconnected peer = proc a -> do 
      e <- peerDisconnected peer -< ()
      traceEvent (const "Disconnected from server") -< e
      rSwitch (runActor' (playGame peer)) -< (a, const disconnected <$> e)

    disconnected = pure Nothing

playGame :: Peer -> AppActor GameId a (Maybe Game)
playGame peer = actorMaker $ proc (_, mg) -> do 
  p <- runActor' $ playerActor initialPlayer -< ()
  c <- runActor' $ cameraWire initialCamera -< ()
  rps <- case mg of 
    Nothing -> returnA -< []
    Just g -> processRemotePlayers -< g
  forceNF -< Just $! case mg of 
    Nothing -> Game {
        gameId = gid
      , gamePlayer = p
      , gameCamera = c
      , gameRemotePlayers = rps
      , gameAddPlayers = []
      , gameRemovePlayers = []
      }
    Just g -> g {
        gamePlayer = p
      , gameCamera = c
      , gameRemotePlayers = rps
      , gameAddPlayers = []
      , gameRemovePlayers = []
      }
  where 
  -- | Static id of global space
  gid = GameId 0

  -- | Maker of startup camera
  initialCamera i = Camera i 0 0 (-0.1)
  -- | Maker of startup player
  initialPlayer i = Player {
      playerId = i 
    , playerPos = 0
    , playerColor = V3 1 0 0
    , playerRot = 0
    , playerSpeed = 0.5
    , playerPeer = peer
    }

  -- | Helper to hide some boring processing of inner state, messages and networking
  actorMaker = netStateActorFixed gid Nothing process
    peer 1 netProcess
  process g _ = g -- ^ Empty message handler

  netProcess _ Nothing _ = Nothing
  netProcess _ (Just g) msg = Just $ case msg of 
    PlayerSpawn pid -> g { gameAddPlayers = fromCounter pid : gameAddPlayers g} 
    PlayerDespawn pid -> g { gameRemovePlayers = fromCounter pid : gameRemovePlayers g} 

  -- | Handles spawing/despawing of other players
  processRemotePlayers :: AppWire Game [RemotePlayer]
  processRemotePlayers = proc g -> do
    adde <- mapE (fmap $ remotePlayerActor peer) . became (not . null) -< gameAddPlayers g
    reme <- became (not . null) -< gameRemovePlayers g
    dynCollection [] -< ((), adde, reme)
