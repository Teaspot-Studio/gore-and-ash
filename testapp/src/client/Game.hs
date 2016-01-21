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
import Data.Text (pack)
import Prelude hiding (id, (.))
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
import Game.Shared

-- | Entry point of the game, controls global game stages
mainWire :: AppWire a (Maybe Game)
mainWire = waitConnection
  where
    -- | Game stage before connection established
    waitConnection = dSwitch $ proc _ -> do 
      e <- mapE seqLeftHead . peersConnected -< ()
      traceEvent (const "Connected to server") -< e
      returnA -< (Nothing, waitPlayerId <$> e) 

    -- | Helper to get left most element from sequence or die
    seqLeftHead s = case S.viewl s of 
      S.EmptyL -> error "seqLeftHead: empty sequence"
      (h S.:< _) -> h 

    -- | After connection succeded, get important data to begin game
    waitPlayerId peer = switch $ proc _ -> do 
      -- Send request for player id
      emsg <- now -< PlayerRequestId
      peerSendIndexed peer (ChannelID 0) globalGameId ReliableMessage -< emsg
      traceEvent (const "Waiting for player id") -< emsg
      
      -- Waiting for respond with player id
      e <- mapE seqLeftHead . filterMsgs isPlayerResponseId . peerIndexedMessages peer (ChannelID 0) globalGameId -< () 
      traceEvent (\(PlayerResponseId i _ _) -> "Got player id: " <> pack (show i)) -< e
      
      -- When recieved switch to next stage
      let nextWire = (\(PlayerResponseId i bulletsColId playersColId) -> untilDisconnected peer (PlayerId i) (fromCounter bulletsColId) (fromCounter playersColId)) <$> e
      returnA -< (Nothing, nextWire)

    -- | Main game stage, actual playing
    untilDisconnected peer pid bulletsColId playersColId = switch $ proc _ -> do 
      e <- peerDisconnected peer -< ()
      traceEvent (const "Disconnected from server") -< e
      g <- runActor' (playGame pid peer bulletsColId playersColId) -< ()
      returnA -< (g, const disconnected <$> e)

    -- | Final stage, we are disconnected
    disconnected = pure Nothing

-- | Controller of main game stage when actual playing is happen
playGame :: PlayerId -> Peer -> RemActorCollId -> RemActorCollId -> AppActor GameId a (Maybe Game)
playGame pid peer bulletsColId playersColId = makeFixedActor globalGameId $ stateWire Nothing $ proc (_, mg) -> do 
  c <- runActor' $ cameraWire initialCamera -< ()
  ex <- exitCheck -< ()
  (ps, bs) <- case mg of 
    Nothing -> returnA -< (H.empty, H.empty)
    Just g -> do 
      ps <- processPlayers playersColId -< g
      bs <- processBullets bulletsColId -< g
      returnA -< (ps, bs)

  forceNF -< Just $! case mg of 
    Nothing -> Game {
        gameId = globalGameId
      , gamePlayer = H.lookup pid ps
      , gameCamera = c
      , gamePlayers = ps
      , gameBullets = bs
      , gameExit = ex
      }
    Just g -> g {
        gamePlayer = H.lookup pid ps
      , gameCamera = c
      , gamePlayers = ps
      , gameBullets = bs
      , gameExit = ex
      }
  where
  -- | Check if user or system wants us to die
  exitCheck = proc _ -> do 
    e <- windowClosed mainWindowName -< ()
    q <- liftGameMonad sdlQuitEventM -< ()
    returnA -< event False (const True) e || q

  -- | Maker of startup camera
  initialCamera i = Camera i 0 0.1

  -- | Handles spawing/despawing of other players
  processPlayers :: RemActorCollId -> AppWire Game PlayerMap
  processPlayers cid = proc g -> do
    ps <- runActor' $ remoteActorCollectionClient cid peer makePlayerActor -< gameCamera g
    returnA -< mapFromSeq $ fmap playerId ps `S.zip` ps
    where
      makePlayerActor i = if i == pid 
        then playerActor peer pid
        else remotePlayerActor peer i

  -- | Handles spawing/despawing of bullets
  processBullets :: RemActorCollId -> AppWire Game BulletMap
  processBullets cid = proc g -> do 
    bs <- runActor' $ remoteActorCollectionClient cid peer (bulletActor peer) -< g
    returnA -< mapFromSeq $ fmap bulletId bs `S.zip` bs