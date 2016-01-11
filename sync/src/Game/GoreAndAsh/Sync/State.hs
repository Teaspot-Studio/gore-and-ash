module Game.GoreAndAsh.Sync.State(
    SyncState(..)
  , SyncRole(..)
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

-- | Defines behavior in synchronization for actor ids
data SyncRole = 
    SyncMaster -- ^ Registers types of actors
  | SyncSlave -- ^ Always ask for ids from other nodes
  deriving (Generic, Eq, Show, Enum)

instance NFData SyncRole 

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
, syncScheduledMessages :: !(H.HashMap Peer (S.Seq (String, ChannelID, Word64 -> Message)))
  -- | Flag that enables detailed logging
, syncLogging :: !Bool
  -- | Current model of synchronization
, syncRole :: !SyncRole
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
  , syncLogging = False
  , syncRole = SyncMaster
  }