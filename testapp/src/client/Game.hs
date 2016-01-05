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
import Data.Text (pack)
import GHC.Generics (Generic)
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
    waitConnection = switch $ proc _ -> do 
      e <- mapE head . peersConnected -< ()
      traceEvent (const "Connected to server") -< e
      returnA -< (Nothing, waitPlayerId <$> e) 

    waitPlayerId peer = switch $ proc _ -> do 
      emsg <- now -< PlayerRequestId
      peerSendIndexed peer (ChannelID 0) globalGameId ReliableMessage -< emsg
      traceEvent (const "Waiting for player id") -< emsg
      
      e <- mapE head . filterMsgs isPlayerSpawn . peerIndexedMessages peer (ChannelID 0) globalGameId -< () 
      traceEvent (\(PlayerSpawn i) -> "Got player id: " <> pack (show i)) -< e
      
      let nextWire = (\(PlayerSpawn i) -> untilDisconnected peer $ PlayerId i) <$> e
      returnA -< (Nothing, nextWire)

    untilDisconnected peer pid = switch $ proc _ -> do 
      e <- peerDisconnected peer -< ()
      traceEvent (const "Disconnected from server") -< e
      g <- runActor' (playGame pid peer) -< ()
      returnA -< (g, const disconnected <$> e)

    disconnected = pure Nothing

playGame :: PlayerId -> Peer -> AppActor GameId a (Maybe Game)
playGame pid peer = actorMaker $ proc (_, mg) -> do 
  p <- runActor' $ playerActor pid peer -< ()
  c <- runActor' $ cameraWire initialCamera -< ()
  rps <- case mg of 
    Nothing -> returnA -< []
    Just g -> processRemotePlayers -< g
  forceNF -< Just $! case mg of 
    Nothing -> Game {
        gameId = globalGameId
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

  -- | Maker of startup camera
  initialCamera i = Camera i 0 0 (-0.1)

  -- | Helper to hide some boring processing of inner state, messages and networking
  actorMaker = netStateActorFixed globalGameId Nothing process
    peer 1 netProcess
  process g _ = g -- ^ Empty message handler

  netProcess _ Nothing _ = Nothing
  netProcess _ (Just g) msg = Just $ case msg of 
    PlayerSpawn i -> g { gameAddPlayers = fromCounter i : gameAddPlayers g} 
    PlayerDespawn i -> g { gameRemovePlayers = fromCounter i : gameRemovePlayers g} 
    _ -> g 

  -- | Handles spawing/despawing of other players
  processRemotePlayers :: AppWire Game [RemotePlayer]
  processRemotePlayers = proc g -> do
    adde <- became (not . null) -< gameAddPlayers g
    reme <- became (not . null) -< gameRemovePlayers g
    traceEvent (\ids -> "New remote players: " <> pack (show ids)) -< adde
    traceEvent (\ids -> "Removed remote players: " <> pack (show ids)) -< reme
    dDynCollection [] -< ((), fmap (remotePlayerActor peer) <$> adde, reme)