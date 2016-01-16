module Game.GoreAndAsh.Sync.Remote.Actor(
    RemoteActor(..)
  , RemActorId(..)
  , RemActorNetMessage(..)
  ) where

import Control.Wire
import GHC.Generics
import Prelude hiding ((.), id)

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Sync.Message 

-- | API to support automatic synchronization of actors between client and server
class NetworkMessage i => RemoteActor i a | i -> a, a -> i where
  type RemoteActorState i :: *
  type RemoteActorId a :: *
  
-- | Id of synchronization actor build over another actor
newtype RemActorId i = RemActorId { unRemActorId :: i }
  deriving (Show, Eq, Ord, Generic)

-- | Stub for local remote actor API
data RemActorMessage i 

instance RemoteActor i a => ActorMessage (RemActorId i) where
  type ActorMessageType (RemActorId i) = RemActorMessage i
  toCounter = toCounter . unRemActorId
  fromCounter = RemActorId . fromCounter

-- | Network protocol of synchronization actor
data RemActorNetMessage i = RemActorNetMessageStub

instance RemoteActor i a => NetworkMessage (RemActorId i) where
  type NetworkMessageType (RemActorId i) = RemActorNetMessage i