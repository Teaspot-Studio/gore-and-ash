module Game.GoreAndAsh.Sync.Remote.Sync(
  -- | Remote actor API
    Sync(..)
  , FullSync
  , RemoteActor(..)
  , noSync
  , clientSide
  , serverSide
  , condSync
  , syncReject
  -- | Helpers for conditional synchronization
  , fieldChanges
  , fieldChangesWithin
  -- | Dictionary utils
  , Dict(..)
  , encodish
  , decodish
  ) where

import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Serialize
import Data.Word 
import qualified Data.ByteString as BS 
import Prelude hiding (id, (.))

import Game.GoreAndAsh
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync.Message 

-- | Reify typeclass to dictionary
data Dict ctxt where
  Dict :: ctxt => Dict ctxt

-- | Use serialize dictionary to call @encode@
encodish :: Dict (Serialize a) -> a -> BS.ByteString 
encodish Dict = encode 

-- | Use serialize dictionary to call @decode@
decodish :: Dict (Serialize a) -> BS.ByteString -> Either String a
decodish Dict = decode 

-- | Special monad that keeps info about synchronization logic 
-- between client and server for type @a@. Remote collection 
-- uses the description to generate special code to automatic
-- synchronization of shared actor state.
--
-- - Parameter @m@ means underlying game monad, that will be used during synchronization
-- 
-- - Parameter @i@ means actor unique id type
--
-- - Parameter @s@ means actor state that is beeing syncing. As soon as you crafted
--   @Sync i s s@ it means you defined full description how to sync actor state.
--
-- - Parameter @a@ is actual value type that the @Sync@ value is describing synchronization for.
--   As soon as you crafted @Sync i s s@ it means you defined full description how to sync actor state.
data Sync m i s a where
  SyncPure :: a -> Sync m i s a -- ^ Statically known value
  SyncNone :: (s -> a) -> Sync m i s a -- ^ No synchronization, take local value
  SyncClient :: Dict (Eq a, Serialize a, RemoteActor i s) -> Peer -> !Word64 -> (s -> a) -> Sync m i s a -- ^ The value is controlled by client and synched to server.
  SyncServer :: Dict (Serialize a, RemoteActor i s) -> !Word64 -> (s -> a) -> Sync m i s a -- ^ The value is controlled by server and synched to clients.
  SyncCond :: GameWire m s (Event ()) -> (s -> a) -> Sync m i s a -> Sync m i s a -- ^ Conditional synchronization
  SyncReject :: Dict (Serialize a, RemoteActor i s) -> GameWire m (s, a) (Event a) -> !Word64 -> Sync m i s a -> Sync m i s a -- ^ Validate synchronized value, rollback if failed
  SyncApp :: Sync m i s (a -> b) -> Sync m i s a -> Sync m i s b -- ^ Applicative application of actions

instance Functor (Sync m i s) where
  fmap f s = case s of 
    SyncPure a -> SyncPure (f a)
    _ -> SyncApp (SyncPure f) s

instance Applicative (Sync m i s) where
  pure = SyncPure
  sf <*> s = SyncApp sf s

-- | Type synonim for those Sync DSL programs that defines full synchronization of actor state
type FullSync m i s = Sync m i s s

-- | API to support automatic synchronization of actors between client and server
class NetworkMessage i => RemoteActor i a | i -> a, a -> i where
  type RemoteActorState i :: *
  type RemoteActorId a :: *

-- | Perphoms no synchronization, the sync primitive returns local value of field
noSync :: (s -> a) -- ^ Getter of the field
  -> Sync m i s a
noSync = SyncNone 

-- | Declares that state field is client side, i.e. it is produced in client actor
-- and then sent to server. For peers that are not equal to specified (owner of the field)
-- the sync behavior acts as @serverSide@.
clientSide :: (Eq a, Serialize a, RemoteActor i s)
  => Peer -- ^ Which peer controls the field, sync messages from other peers are not processed
  -> Word64 -- ^ Field id, other side actor should define @clientSide@ with matching id
  -> (s -> a) -- ^ Field getter
  -> Sync m i s a
clientSide peer !w getter = SyncClient Dict peer w getter

-- | Declares that state field is server side, i.e. it is produced in server actor
-- and then sent to all clients.
serverSide :: (Serialize a, RemoteActor i s)
  => Word64 -- ^ Field id, other side actor should define @serverSide@ with matching id
  -> (s -> a) -- ^ Field getter
  -> Sync m i s a
serverSide !w getter = SyncServer Dict w getter

-- | Makes synchronization appear only when given wire produces an event
condSync :: Monad m => GameWire m s (Event b) -- ^ Wire that produces events when sync should be done
  -> (s -> a) -- ^ Field getter
  -> Sync m i s a -- ^ Sub action that should be done when sync event is produced
  -> Sync m i s a
condSync w getter ms = SyncCond (mapE (const ()) . w) getter ms

-- | Produces event when given field is changed
fieldChanges :: Eq a => (s -> a) -- ^ Field getter
  -> GameWire m s (Event a)
fieldChanges getter = mkSFN $ \s -> let a = getter s in a `seq` (Event a, go a)
  where
    go a = mkSFN $ \s -> if a == getter s 
      then (NoEvent, go a)
      else let a2 = getter s in a2 `seq` (Event a2, go a2)

-- | Produces event when given field is changed
fieldChangesWithin :: (Num a, Ord a) => (s -> a) -- ^ Field getter
  -> a -- ^ Delta, variation greater than the value is treated as change
  -> GameWire m s (Event a)
fieldChangesWithin getter delta = mkSFN $ \s -> let a = getter s in a `seq` (Event a, go a)
  where
    go a = mkSFN $ \s -> let a2 = getter s in if abs (a - a2) < delta
      then (NoEvent, go a)
      else a2 `seq` (Event a2, go a2)

-- | There are sometimes net errors or malicios data change in remote actor,
-- the action provides you ability to reject incorrect values and resync remote
-- actor to fallback value.
syncReject :: (Serialize a, RemoteActor i s)
  => GameWire m (s, a) (Event a) -- ^ Fires event when the synced value is invalid, event carries new value that should be placed and sended to remote peer
  -> Word64 -- ^ Id of field to resync at remote host when failed
  -> Sync m i s a -- ^ Sub action that produces synced values for first argument
  -> Sync m i s a
syncReject w wid ms = SyncReject Dict w wid ms