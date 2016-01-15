module Game.GoreAndAsh.Sync.Remote.Collection(
    RemActorCollId(..)
  , remoteActorCollectionServer
  , remoteActorCollectionClient
  ) where

import Control.DeepSeq
import Control.Wire
import Data.Filterable
import Data.Serialize
import GHC.Generics
import Prelude hiding ((.), id)
import qualified Data.Foldable as F 

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging 
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync.API
import Game.GoreAndAsh.Sync.Message 

import Game.GoreAndAsh.Sync.Remote.Actor

-- | Unique id space for remote collections actors
newtype RemActorCollId = RemActorCollId { unRemActorCollId :: Int } 
  deriving (Show, Eq, Ord, Generic)

-- | Stub for local collection API
data RemActorCollMessage

instance ActorMessage RemActorCollId where
  type ActorMessageType RemActorCollId = RemActorCollMessage
  toCounter = unRemActorCollId
  fromCounter = RemActorCollId

-- | Comminucation protocol between client and server side collections
data RemActorCollNetMessage =
  -- | Sent to client when server collection adds new actor
    RemActorCollNetSpawn !Int 
  -- | Sent to client when server collection removes specific actor
  | RemActorCollNetDespawn !Int 
  -- | Sent when remote collection is set up to get actors that were 
  -- created before the collection creation
  | RemActorCollRequestOthers 
  deriving (Generic)

instance NFData RemActorCollNetMessage
instance Serialize RemActorCollNetMessage

instance NetworkMessage RemActorCollId where
  type NetworkMessageType RemActorCollId = RemActorCollNetMessage

-- | Server side collection of network actors that are automatically
-- synchronized to remote clients.
-- 
-- Second wire input is event with new actors to add to the collection
-- Third wire input is event with id of actor to delete from the collection
remoteActorCollectionServer :: (SyncMonad m, LoggingMonad m, ActorMonad m, NetworkMonad m, Eq i, RemoteActor i b, DynCollection c, FilterConstraint c (GameWireIndexed m i a b), FilterConstraint c (Either () b), F.Foldable c2) 
  => c (GameActor m i a b) -- ^ Initial set of actors
  -> GameActor m RemActorCollId (a, Event (c (GameActor m i a b)), Event (c2 i)) (c b)
remoteActorCollectionServer initalActors = makeActor $ \_ -> proc (a, addEvent, remEvent) -> do 
  dynCollection initalActors -< (a, addEvent, remEvent)

-- | Internal state of client side collection of remote actors
data ClientRemCollState c i = ClientRemCollState {
  clientRemCollNewActors :: !(c i)
, clientRemCollDelActors :: ![i]
}

emptyClientRemCollState :: DynCollection c => ClientRemCollState c i 
emptyClientRemCollState = ClientRemCollState {
    clientRemCollNewActors = emptyDynColl
  , clientRemCollDelActors = emptyDynColl
  }

-- | Client side collection of network actors that are automatically
-- synchronized to remote clients.
remoteActorCollectionClient :: forall m i a b c . (SyncMonad m, LoggingMonad m, ActorMonad m, NetworkMonad m, Eq i, RemoteActor i b, DynCollection c, FilterConstraint c (GameWireIndexed m i a b), FilterConstraint c (Either () b), DynConsConstr c i) 
  => RemActorCollId -- ^ Corresponding server collection id
  -> Peer -- ^ Server peer
  -> (i -> GameActor m i a b) -- ^ How to construct client side actors
  -> GameActor m RemActorCollId a (c b)
remoteActorCollectionClient cid peer mkActor = makeFixedActor cid $ proc a -> do
  cs <- peerProcessIndexed peer (ChannelID 0) cid processNet -< emptyClientRemCollState
  addEvent <- became (not . null) -< clientRemCollNewActors cs
  remEvent <- became (not . null) -< clientRemCollDelActors cs
  dynCollection emptyDynColl -< (a, fmap mkActor <$> addEvent, remEvent)
  where
  processNet :: ClientRemCollState c i -> RemActorCollNetMessage -> ClientRemCollState c i
  processNet cs msg = case msg of
    RemActorCollNetSpawn i -> cs { clientRemCollNewActors = fromCounter i `consDynColl` clientRemCollNewActors cs }
    RemActorCollNetDespawn i -> cs { clientRemCollDelActors = fromCounter i : clientRemCollDelActors cs }
    RemActorCollRequestOthers -> cs