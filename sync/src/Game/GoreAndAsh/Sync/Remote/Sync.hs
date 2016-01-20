module Game.GoreAndAsh.Sync.Remote.Sync(
    Sync(..)
  , Dict(..)
  , encodish
  , decodish
  , RemoteActor(..)
  , syncClient
  , syncServer
  , syncCond
  -- | Helpers for conditional synchronization
  , fieldChanges
  , fieldChangesWithin
  ) where

import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Serialize
import Data.Word 
import qualified Data.ByteString as BS 
import Prelude hiding (id, (.))

import Game.GoreAndAsh
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
-- - Parameter @i@ means actor unique id type
-- 
-- - Parameter @m@ means underlying game monad, that will be used during synchronization
--
-- - Parameter @s@ means actor state that is beeing syncing. As soon as you crafted
--   @Sync i s s@ it means you defined full description how to sync actor state.
--
-- - Parameter @a@ is actual value type that the @Sync@ value is describing synchronization for.
--   As soon as you crafted @Sync i s s@ it means you defined full description how to sync actor state.
data Sync i m s a where
  SyncPure :: a -> Sync i m s a -- ^ Statically known value
  SyncClient :: Dict (Serialize a, RemoteActor i a) -> !Word64 -> (s -> a) -> Sync i m s a -- ^ The value is controlled by client and synched to server.
                                                                                         -- There should be only one client actor to proper semantic
  SyncServer :: Dict (Serialize a, RemoteActor i a) -> !Word64 -> (s -> a) -> Sync i m s a -- ^ The value is controlled by server and synched to clients.
  SyncCond :: GameWire m s (Event ()) -> (s -> a) -> Sync i m s a -> Sync i m s a -- ^ Conditional synchronization
  SyncApp :: Sync i m s (a -> b) -> Sync i m s a -> Sync i m s b -- ^ Applicative application of actions

instance Functor (Sync i m s) where
  fmap f s = case s of 
    SyncPure a -> SyncPure (f a)
    _ -> SyncApp (SyncPure f) s

instance Applicative (Sync i m s) where
  pure = SyncPure
  sf <*> s = SyncApp sf s

-- | API to support automatic synchronization of actors between client and server
class NetworkMessage i => RemoteActor i a | i -> a, a -> i where
  type RemoteActorState i :: *
  type RemoteActorId a :: *
  
  -- | Description how to sync actor state
  actorSync :: Sync i m a a

-- | Declares that state field is client side, i.e. it is produced in client actor
-- and then sent to server. The correct operation of the behavior applied on server
-- actor assumes that there is no other clients that syncs the specified.
syncClient :: (Serialize a, RemoteActor i a)
  => Word64 -- ^ Field id, other side actor should define @syncServer@ with matching id
  -> (s -> a) -- ^ Field getter
  -> Sync i m s a
syncClient !w getter = SyncClient Dict w getter

-- | Declares that state field is server side, i.e. it is produced in server actor
-- and then sent to all clients.
syncServer :: (Serialize a, RemoteActor i a)
  => Word64 -- ^ Field id, other side actor should define @syncClient@ with matching id
  -> (s -> a) -- ^ Field getter
  -> Sync i m s a
syncServer !w getter = SyncServer Dict w getter

-- | Makes synchronization appear only when given wire produces an event
syncCond :: Monad m => GameWire m s (Event b) -- ^ Wire that produces events when sync should be done
  -> (s -> a) -- ^ Field getter
  -> Sync i m s a -- ^ Sub action that should be done when sync event is produced
  -> Sync i m s a
syncCond w getter ms = SyncCond (mapE (const ()) . w) getter ms

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