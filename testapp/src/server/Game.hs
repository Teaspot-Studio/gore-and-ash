{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game(
    mainWire
  , Game(..)
  ) where

import Control.Wire
import Data.Maybe 
import Data.Text (pack)
import Linear
import Prelude hiding (id, (.))
import qualified Data.Foldable as F 
import qualified Data.HashMap.Strict as H 

import Game.Bullet 
import Game.Core
import Game.Data
import Game.Player
import Game.Shared

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync 

-- | Wire that returns next player color
playerColors :: AppWire (Event a) (V3 Double)
playerColors = dDispense [
    V3 1 0 0
  , V3 0 1 0
  , V3 0 0 1
  , V3 1 1 0
  , V3 1 0 1
  , V3 0 1 1
  ]

mainWire :: AppActor GameId a Game
mainWire = makeFixedActor globalGameId $ stateWire initGame $ proc (_, g) -> do 
  forceNF . processBullets . processPlayers -< g
  where 
    -- | Game at start of the simulation
    initGame = Game {
        gameId = globalGameId
      , gamePlayers = H.empty
      , gamePlayerPeers = H.empty
      , gameBullets = H.empty
      , gameBulletColId = fromCounter (-1)
      }

    -- | Handles process of players connection and disconnections
    processPlayers :: AppWire Game Game
    processPlayers = proc g -> do 
      conEvent <- peersConnected -< ()
      col <- playerColors -< conEvent
      let addEvent = F.toList . fmap (spawnPlayer col) <$> conEvent

      disEvent <- peersDisconnected -< ()
      remEvent <- mapE F.toList . filterJustLE -< fmap (despawnPlayer g) <$> disEvent

      traceEvent (const "New player connected") -< addEvent -- Player id is not ready yet :(
      traceEvent (\i -> "Player " <> (pack . show) i <> " disconnected") -< remEvent

      notifyAboutDespawn -< (g, remEvent)

      ps <- dynCollection [] -< (g, addEvent, remEvent)
      returnA -< g {
          gamePlayers = H.fromList $ fmap playerId  ps `zip` ps
        , gamePlayerPeers = H.fromList $ fmap playerPeer ps `zip` fmap playerId ps
        }

    -- | Spawns new player from peer (creates new arrow)
    spawnPlayer :: V3 Double -> Peer -> AppActor PlayerId Game Player
    spawnPlayer c p = playerActor $ \i -> Player {
        playerId = i
      , playerPos = 0
      , playerColor = c
      , playerRot = 0
      , playerPeer = p
      , playerSpeed = 6
      , playerSize = 1
      }

    -- | Detects player id by peer for despawning
    despawnPlayer :: Game -> Peer -> Maybe PlayerId
    despawnPlayer Game{..} p = H.lookup p gamePlayerPeers

    -- | Notify another players about disconnected
    notifyAboutDespawn :: AppWire (Game, Event [PlayerId]) ()
    notifyAboutDespawn = proc (Game{..}, epids) -> do 
      let ps = H.elems gamePlayers -- Players we should notify
          msgs = (\pids -> concat $ mkMsgs pids <$> ps) <$> epids 
      peerSendIndexedManyDyn (ChannelID 0) ReliableMessage -< msgs
      returnA -< () 
      where
        -- | Make messages about players despawn for given player
        mkMsgs :: [PlayerId] -> Player -> [(Peer, GameId, GameNetMessage)]
        mkMsgs pids p = if playerId p `elem` pids 
          then []
          else mkMsg <$> pids
          where
          mkMsg pid = (playerPeer p, globalGameId, PlayerDespawn $ toCounter pid)

    -- | Handle bullets actors
    processBullets :: AppWire Game Game 
    processBullets = proc g -> do 
      addEvent <- mapE (fmap (bulletActor . newBullet) . F.toList) . actorMessages globalGameId isGameSpawnBullet -< ()
      traceEvent (pack . show) . actorMessages globalGameId isGameSpawnBullet -< ()
      remEvent <- mapE (const []) . never -< ()
      (bs, i) <- runActor $ remoteActorCollectionServer [] -< (g, addEvent, remEvent)
      returnA -< g {
          gameBullets = H.fromList $ fmap bulletId bs `zip` bs
        , gameBulletColId = i
        }
      where
        newBullet (GameSpawnBullet pos vel owner) i = Bullet {
            bulletId = i 
          , bulletPos = pos
          , bulletVel = vel
          , bulletOwner = owner
          }
        -- newBullet _ _ = error "newBullet: wrong message"