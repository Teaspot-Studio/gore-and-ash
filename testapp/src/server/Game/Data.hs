{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Data(
    Game(..)
  , GameMessage(..)
  , PlayerMap
  , PlayerPeerMap
  , BulletsMap
  -- | Helpers
  , isGameSpawnBullet
  , isGameDeleteBullet
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import Prelude hiding (id, (.))
import qualified Data.HashMap.Strict as H 
import Linear 

import Game.Bullet.Data
import Game.Player.Data
import Game.Shared

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync 

type PlayerMap = H.HashMap PlayerId Player
type PlayerPeerMap = H.HashMap Peer PlayerId 
type BulletsMap = H.HashMap BulletId Bullet 

-- | Server local messages to game actor
data GameMessage = 
    -- | Spawn bullet at given pos with given velocity and owner
    GameSpawnBullet !(V2 Double) !(V2 Double) !PlayerId 
    -- | Deletes bullet with specified id
  | GameDeleteBullet !BulletId
  deriving (Generic, Show)

instance ActorMessage GameId where
  type ActorMessageType GameId = GameMessage 
  toCounter = unGameId
  fromCounter = GameId

instance NetworkMessage GameId where 
  type NetworkMessageType GameId = GameNetMessage

data Game = Game {
  gameId :: !GameId
, gamePlayers :: !PlayerMap
, gamePlayerPeers :: !PlayerPeerMap
, gameBullets :: !BulletsMap
, gameBulletColId :: !RemActorCollId 
, gamePlayerColId :: !RemActorCollId
} deriving (Generic)

instance NFData Game 

-- | Detect specific message
isGameSpawnBullet :: GameMessage -> Bool
isGameSpawnBullet m = case m of 
  GameSpawnBullet _ _ _ -> True
  _ -> False

-- | Detect specific message
isGameDeleteBullet :: GameMessage -> Bool
isGameDeleteBullet m = case m of 
  GameDeleteBullet _ -> True
  _ -> False