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
import Control.Wire.Unsafe.Event (event)
import Data.Text (pack)
import GHC.Generics (Generic)
import Prelude hiding (id, (.))
import qualified Data.Sequence as S 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync 

import Consts
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
, gameExit :: !Bool
} deriving (Generic)

instance NFData Game 

mainWire :: AppWire a (Maybe Game)
mainWire = waitConnection
  where
    waitConnection = switch $ proc _ -> do 
      e <- mapE seqLeftHead . peersConnected -< ()
      traceEvent (const "Connected to server") -< e
      returnA -< (Nothing, waitPlayerId <$> e) 

    seqLeftHead s = case S.viewl s of 
      S.EmptyL -> error "seqLeftHead: empty sequence"
      (h S.:< _) -> h 

    waitPlayerId peer = switch $ proc _ -> do 
      emsg <- now -< PlayerRequestId
      peerSendIndexed peer (ChannelID 0) globalGameId ReliableMessage -< emsg
      traceEvent (const "Waiting for player id") -< emsg
      
      e <- mapE seqLeftHead . filterMsgs isPlayerResponseId . peerIndexedMessages peer (ChannelID 0) globalGameId -< () 
      traceEvent (\(PlayerResponseId i) -> "Got player id: " <> pack (show i)) -< e
      
      let nextWire = (\(PlayerResponseId i) -> untilDisconnected peer $ PlayerId i) <$> e
      returnA -< (Nothing, nextWire)

    untilDisconnected peer pid = switch $ proc _ -> do 
      e <- peerDisconnected peer -< ()
      traceEvent (const "Disconnected from server") -< e
      g <- runActor' (playGame pid peer) -< ()
      returnA -< (g, const disconnected <$> e)

    disconnected = pure Nothing

playGame :: PlayerId -> Peer -> AppActor GameId a (Maybe Game)
playGame pid peer = do
  peerSendIndexedM peer (ChannelID 0) globalGameId ReliableMessage PlayerRequestOthers
  actorMaker $ proc (_, mg) -> do 
    c <- runActor' $ cameraWire initialCamera -< ()
    p <- runActor' $ playerActor pid peer -< c
    ex <- exitCheck -< ()
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
        , gameExit = ex
        }
      Just g -> g {
          gamePlayer = p
        , gameCamera = c
        , gameRemotePlayers = rps
        , gameAddPlayers = []
        , gameRemovePlayers = []
        , gameExit = ex
        }
  where
  exitCheck = proc _ -> do 
    e <- windowClosed mainWindowName -< ()
    q <- liftGameMonad sdlQuitEventM -< ()
    returnA -< event False (const True) e || q

  -- | Maker of startup camera
  initialCamera i = Camera i 0 (-0.1)

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