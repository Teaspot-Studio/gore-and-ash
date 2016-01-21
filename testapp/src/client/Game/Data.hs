{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Data(
    BulletMap
  , PlayerMap
  , Game(..)
  ) where

import Control.DeepSeq
import Game.Shared
import GHC.Generics
import qualified Data.HashMap.Strict as H 

import Game.Bullet.Data
import Game.Camera 
import Game.Player.Data 

import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Sync 

type BulletMap = H.HashMap BulletId Bullet
type PlayerMap = H.HashMap PlayerId Player 

data Game = Game {
  gameId :: !GameId
, gamePlayer :: !(Maybe Player)
, gameCamera :: !Camera
, gamePlayers :: !PlayerMap
, gameBullets :: !BulletMap
, gameExit :: !Bool
} deriving (Generic)

instance NFData Game 

data GameMessage 

instance ActorMessage GameId where
  type ActorMessageType GameId = GameMessage 
  toCounter = unGameId
  fromCounter = GameId

instance NetworkMessage GameId where 
  type NetworkMessageType GameId = GameNetMessage
