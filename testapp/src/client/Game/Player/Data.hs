module Game.Player.Data(
    Player(..)
  , PlayerId(..)
  , PlayerMessage(..)
  , PlayerNetMessage(..)
  ) where

import Control.DeepSeq
import Data.Typeable
import GHC.Generics (Generic)
import Linear
import Prelude hiding (id, (.))

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync

import Game.Player.Shared

data Player = Player {
  playerId :: !PlayerId
, playerPos :: !(V2 Double)
, playerColor :: !(V3 Double) 
, playerRot :: !Double
, playerSpeed :: !Double
, playerPeer :: !Peer 
} deriving (Generic, Show)

instance NFData Player 

newtype PlayerId = PlayerId { unPlayerId :: Int } deriving (Eq, Show, Generic)
instance NFData PlayerId 

data PlayerMessage = PlayerMessageStub deriving (Typeable, Generic)
instance NFData PlayerMessage 

instance ActorMessage PlayerId where
  type ActorMessageType PlayerId = PlayerMessage
  toCounter = unPlayerId
  fromCounter = PlayerId

instance NetworkMessage PlayerId where 
  type NetworkMessageType PlayerId = PlayerNetMessage