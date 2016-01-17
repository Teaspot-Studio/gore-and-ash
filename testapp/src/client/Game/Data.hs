{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Data(
    BulletMap
  , Game(..)
  ) where

import Control.DeepSeq
import Game.Shared
import GHC.Generics
import qualified Data.HashMap.Strict as H 

import Game.Bullet.Data
import Game.Camera 
import Game.Player.Data 
import Game.RemotePlayer.Data

import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Sync 

type BulletMap = H.HashMap BulletId Bullet

data Game = Game {
  gameId :: !GameId
, gamePlayer :: !Player 
, gameCamera :: !Camera
, gameRemotePlayers :: ![RemotePlayer] 
, gameAddPlayers :: ![PlayerId]
, gameRemovePlayers :: ![PlayerId]
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
