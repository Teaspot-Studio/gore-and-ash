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
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

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
      , gamePlayerColId = fromCounter (-1)
      }

    -- | Handles process of players connection and disconnections
    processPlayers :: AppWire Game Game
    processPlayers = proc g -> do 
      conEvent <- peersConnected -< ()
      col <- playerColors -< conEvent
      let addEvent = fmap (spawnPlayer col) <$> conEvent

      disEvent <- peersDisconnected -< ()
      remEvent <- filterJustLE -< fmap (despawnPlayer g) <$> disEvent

      traceEvent (const "New player connected") -< addEvent -- Player id is not ready yet :(
      traceEvent (\i -> "Player " <> (pack . show) i <> " disconnected") -< remEvent

      (ps, i) <- runActor $ remoteActorCollectionServer S.empty -< (g, addEvent, remEvent)
      returnA -< g {
          gamePlayers = mapFromSeq $ fmap playerId  ps `S.zip` ps
        , gamePlayerPeers = mapFromSeq $ fmap playerPeer ps `S.zip` fmap playerId ps
        , gamePlayerColId = i
        }

    -- | Spawns new player from peer (creates new arrow)
    spawnPlayer :: V3 Double -> Peer -> AppActor PlayerId Game Player
    spawnPlayer c p = playerActor $ \i -> Player {
        playerId = i
      , playerPos = 0
      , playerColor = c
      , playerRot = 0
      , playerPeer = p
      , playerSpeed = 15
      , playerSize = 1
      }

    -- | Detects player id by peer for despawning
    despawnPlayer :: Game -> Peer -> Maybe PlayerId
    despawnPlayer Game{..} p = H.lookup p gamePlayerPeers

    -- | Handle bullets actors
    processBullets :: AppWire Game Game 
    processBullets = proc g -> do 
      addEvent <- mapE (fmap (bulletActor . newBullet)) . actorMessages globalGameId isGameSpawnBullet -< ()
      remEvent <- mapE (fmap $ \(GameDeleteBullet i) -> i) . actorMessages globalGameId isGameDeleteBullet -< ()
      (bs, i) <- runActor $ remoteActorCollectionServer S.empty -< (g, addEvent, remEvent)
      returnA -< g {
          gameBullets = mapFromSeq $ fmap bulletId bs `S.zip` bs
        , gameBulletColId = i
        }
      where
        newBullet (GameSpawnBullet pos vel owner) i = Bullet {
            bulletId = i 
          , bulletPos = pos
          , bulletVel = vel
          , bulletOwner = owner
          }
        newBullet _ _ = error "newBullet: wrong message"