{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Player(..)
  , Camera(..)
  , Game(..)
  , AppMonad
  ) where

import Control.Wire
import Control.Wire.Unsafe.Event (event)
import Data.Hashable
import Data.Text (pack)
import Prelude hiding (id, (.))
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.SDL
import Game.GoreAndAsh.Sync 

import Consts
import Game.Bullet 
import Game.Camera 
import Game.Core
import Game.Data
import Game.Player
import Game.RemotePlayer
import Game.Shared

mainWire :: AppWire a (Maybe Game)
mainWire = waitConnection
  where
    waitConnection = dSwitch $ proc _ -> do 
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
      traceEvent (\(PlayerResponseId i _) -> "Got player id: " <> pack (show i)) -< e
      
      let nextWire = (\(PlayerResponseId i bulletsColId) -> untilDisconnected peer (PlayerId i) (fromCounter bulletsColId)) <$> e
      returnA -< (Nothing, nextWire)

    untilDisconnected peer pid bulletsColId = switch $ proc _ -> do 
      e <- peerDisconnected peer -< ()
      traceEvent (const "Disconnected from server") -< e
      g <- runActor' (playGame pid peer bulletsColId) -< ()
      returnA -< (g, const disconnected <$> e)

    disconnected = pure Nothing

playGame :: PlayerId -> Peer -> RemActorCollId -> AppActor GameId a (Maybe Game)
playGame pid peer bulletsColId = do
  peerSendIndexedM peer (ChannelID 0) globalGameId ReliableMessage PlayerRequestOthers
  makeFixedActor globalGameId $ stateWire Nothing $ proc (_, mg_) -> do 
    mg <- peerProcessIndexed peer (ChannelID 0) globalGameId netProcess -< mg_
    c <- runActor' $ cameraWire initialCamera -< ()
    p <- runActor' $ playerActor pid peer -< c
    ex <- exitCheck -< ()
    (rps, bs) <- case mg of 
      Nothing -> returnA -< ([], H.empty)
      Just g -> do 
        rps <- processRemotePlayers -< g
        bs <- processBullets bulletsColId -< g
        returnA -< (rps, bs)

    forceNF -< Just $! case mg of 
      Nothing -> Game {
          gameId = globalGameId
        , gamePlayer = p
        , gameCamera = c
        , gameRemotePlayers = rps
        , gameAddPlayers = []
        , gameRemovePlayers = []
        , gameBullets = bs
        , gameExit = ex
        }
      Just g -> g {
          gamePlayer = p
        , gameCamera = c
        , gameRemotePlayers = rps
        , gameAddPlayers = []
        , gameRemovePlayers = []
        , gameBullets = bs
        , gameExit = ex
        }
  where
  exitCheck = proc _ -> do 
    e <- windowClosed mainWindowName -< ()
    q <- liftGameMonad sdlQuitEventM -< ()
    returnA -< event False (const True) e || q

  -- | Maker of startup camera
  initialCamera i = Camera i 0 (-0.1)

  netProcess Nothing _ = Nothing
  netProcess (Just g) msg = Just $ case msg of 
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
    dDynCollection [] -< (gameCamera g, fmap (remotePlayerActor peer) <$> adde, reme)

  -- | Handles spawing/despawing of bullets
  processBullets :: RemActorCollId -> AppWire Game BulletMap
  processBullets cid = proc g -> do 
    bs <- runActor' $ remoteActorCollectionClient cid peer bulletActor -< g
    returnA -< mapFromSeq $ fmap bulletId bs `S.zip` bs

-- | Construct HashMap from sequence
mapFromSeq :: (Hashable i, Eq i) => S.Seq (i, a) -> H.HashMap i a 
mapFromSeq = F.foldl' (\acc (i, a) -> H.insert i a acc) H.empty