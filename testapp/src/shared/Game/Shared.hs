module Game.Shared(
    GameId(..)
  , GameNetMessage(..)
  ) where

import Control.DeepSeq
import GHC.Generics
import Data.Serialize

import Game.GoreAndAsh.Actor 
import Game.GoreAndAsh.Sync 

-- | Fake id for game global space
newtype GameId = GameId { unGameId :: Int } deriving (Eq, Generic)

instance NFData GameId 

-- | Opaque data for game message
data GameMessage

-- | Generic messages that are not binded to specific actor
data GameNetMessage = 
    PlayerSpawn !Int
  | PlayerDespawn !Int 
  deriving (Generic, Show)

instance NFData GameNetMessage
instance Serialize GameNetMessage

instance ActorMessage GameId where
  type ActorMessageType GameId = GameMessage 
  toCounter = unGameId
  fromCounter = GameId 

instance NetworkMessage GameId where 
  type NetworkMessageType GameId = GameNetMessage
