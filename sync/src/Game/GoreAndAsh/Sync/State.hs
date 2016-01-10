module Game.GoreAndAsh.Sync.State(
    SyncState(..)
  , NetworkMessage(..)
  , SyncServiceMsg(..)
  , emptySyncState
  ) where

import Control.DeepSeq
import Data.Serialize 
import Data.Word 
import GHC.Generics
import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 

import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Actor.TypeRep
import Game.GoreAndAsh.Network 

-- | Extension for actor message, messages that are sent to remote host
class ActorMessage i => NetworkMessage i where
  -- | Corresponding message payload for @i@ identifier, usually ADT
  type NetworkMessageType i :: *

-- | Internal service message for synchronizing ids of actors
data SyncServiceMsg = 
  -- | Request id of actor with specified name
    SyncServiceRequestId !String
  -- | Response with actor id and name
  | SyncServiceResponseId !String !Word64
  -- | Response that given actor is not found
  | SyncServiceResponseNotRegistered !String
  deriving (Generic)

instance Serialize SyncServiceMsg

data SyncState s = SyncState {
  -- | Next module in chain
  syncNextState :: !s
  -- | Mapping from type representation to id
, syncIdMap :: !(H.HashMap HashableTypeRep Word64)
  -- | Reverse mapping from id to type representation
, syncIdMapRev :: !(H.HashMap Word64 HashableTypeRep)
  -- | Next empty id for registering, value zero is service value
, syncNextId :: !Word64
  -- | Messages that are waiting resolving of network id of actor
  -- Actor name and actor local id is stored with the message to sent
, syncScheduledMessages :: !(H.HashMap (Peer, ChannelID) (S.Seq (String, Word64 -> Message)))
} deriving (Generic)

instance NFData s => NFData (SyncState s)

-- | Make empty sync state
emptySyncState :: s -> SyncState s 
emptySyncState s = SyncState {
    syncNextState = s
  , syncIdMap = H.empty 
  , syncIdMapRev = H.empty
  , syncNextId = 1
  , syncScheduledMessages = H.empty
  }