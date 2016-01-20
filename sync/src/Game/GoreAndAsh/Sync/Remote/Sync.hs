module Game.GoreAndAsh.Sync.Remote.Sync(
    Sync(..)
  , Dict(..)
  , encodish
  , decodish
  , RemoteActor(..)
  ) where

import Data.Serialize
import Data.Word 
import qualified Data.ByteString as BS 

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
data Sync i s a where
  SyncPure :: a -> Sync i s a -- ^ Statically known value
  SyncClient :: Dict (Serialize a, RemoteActor i a) -> !Word64 -> (s -> a) -> Sync i s a -- ^ The value is controlled by client and synched to server.
                                                                                         -- There should be only one client actor to proper semantic
  SyncServer :: Dict (Serialize a, RemoteActor i a) -> !Word64 -> (s -> a) -> Sync i s a -- ^ The value is controlled by server and synched to clients.
  SyncApp :: Sync i s (a -> b) -> Sync i s a -> Sync i s b -- ^ Applicative application of actions

instance Functor (Sync i s) where
  fmap f s = case s of 
    SyncPure a -> SyncPure (f a)
    _ -> SyncApp (SyncPure f) s

instance Applicative (Sync i s) where
  pure = SyncPure
  sf <*> s = SyncApp sf s

-- | API to support automatic synchronization of actors between client and server
class NetworkMessage i => RemoteActor i a | i -> a, a -> i where
  type RemoteActorState i :: *
  type RemoteActorId a :: *
  
  -- | Description how to sync actor state
  actorSync :: Sync i a a