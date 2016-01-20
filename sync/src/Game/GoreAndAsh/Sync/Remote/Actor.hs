module Game.GoreAndAsh.Sync.Remote.Actor(
    RemoteActor(..)
  , RemActorId(..)
  , RemActorNetMessage(..)
  , fullSyncServer
  , clientSync
  , serverSync
  ) where

import Control.Monad.Fix 
import Control.Wire
import Control.Wire.Unsafe.Event (event, Event(..))
import Data.Serialize 
import Data.Word
import GHC.Generics
import Prelude hiding ((.), id)
import qualified Data.ByteString as BS 
import qualified Data.Foldable as F 
import qualified Data.Sequence as S

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Sync.Message 
import Game.GoreAndAsh.Sync.Remote.Sync 
import Game.GoreAndAsh.Sync.API 

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
data RemActorNetMessage i = 
    RemActorSyncRequest -- ^ Request full synchronization
  | RemActorSyncValue !Word64 !BS.ByteString -- ^ Carries value of indexed value
  deriving (Generic, Show)

instance Serialize (RemActorNetMessage i)

-- | Filter helper to pass through only RemActorSyncRequest
isRemActorSyncRequest :: RemActorNetMessage i -> Bool 
isRemActorSyncRequest m = case m of 
  RemActorSyncRequest -> True 
  _ -> False 

-- | Filter helper to pass through only RemActorSyncValue
isRemActorSyncValue :: Word64 -> RemActorNetMessage i -> Bool 
isRemActorSyncValue ia m = case m of 
  RemActorSyncValue ib _ -> ia == ib 
  _ -> False 

-- | Helper to construct sync message
mkSyncMessage :: Dict (Serialize a) -> Word64 -> a -> RemActorNetMessage i
mkSyncMessage d w a = RemActorSyncValue w (encodish d a)

-- | Helper to parse sync message
fromSyncMessage :: Dict (Serialize a) -> RemActorNetMessage i -> Maybe a 
fromSyncMessage d m = case m of 
  RemActorSyncRequest -> Nothing
  RemActorSyncValue _ bs -> case decodish d bs of 
    Left _ -> Nothing
    Right a -> Just a

-- | Helper to deserialize only last message
fromSyncMessageLast :: Dict (Serialize a) -> S.Seq (RemActorNetMessage i) -> Maybe a 
fromSyncMessageLast d s = case S.viewr s of 
  S.EmptyR -> Nothing 
  _ S.:> m -> fromSyncMessage d m 

instance RemoteActor i a => NetworkMessage (RemActorId i) where
  type NetworkMessageType (RemActorId i) = RemActorNetMessage i

-- | Helper to run @peerSendIndexedM@ with given dictionary
peerSendRemoteActorMsg :: (ActorMonad m, NetworkMonad m, LoggingMonad m, SyncMonad m)
  => Dict (RemoteActor i a) -- ^ Dictionary from Sync AST
  -> Peer -> ChannelID -> RemActorId i -> MessageType -> RemActorNetMessage i 
  -> GameMonadT m ()
peerSendRemoteActorMsg Dict = peerSendIndexedM

-- | Helper to run @peerIndexedMessages@ with given dictionary
peerListenRemoteActor :: (ActorMonad m, NetworkMonad m, LoggingMonad m, SyncMonad m)
  => Dict (RemoteActor i a) -- ^ Dictionary from Sync AST
  -> Peer -> ChannelID -> RemActorId i
  -> GameWire m () (Event (S.Seq (RemActorNetMessage i)))
peerListenRemoteActor Dict = peerIndexedMessages

-- | Sends all data to remote client
fullSyncServer :: (SyncMonad m, ActorMonad m, NetworkMonad m, LoggingMonad m) 
  => Sync i s a -- ^ Strategy of syncing
  -> Peer -- ^ Which client to send
  -> i -- ^ Id of actor
  -> s -- ^ Value to sync
  -> GameMonadT m a
fullSyncServer ms peer i s = case ms of 
  SyncPure a -> return a -- Client already knows the constant value
  SyncClient _ _ sa -> return (sa s) -- The value is client side
  SyncServer Dict w sa -> do
    let val = sa s
    peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage . mkSyncMessage Dict w $! val
    return val
  SyncApp mf ma -> do 
    a <- fullSyncServer ma peer i s
    f <- fullSyncServer mf peer i s 
    return $ f a

-- | Perform client side synchronization
clientSync :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m) 
  => Sync i s a -- ^ Sync strategy
  -> Peer -- ^ Server connection
  -> i -- ^ Actor id
  -> GameWire m s a -- ^ Synchronizing of client state
clientSync ms peer i = case ms of 
  SyncPure a -> pure a
  SyncClient _ _ sa -> arr sa
  SyncServer Dict w sa -> proc s -> do 
    emsgs <- filterMsgs (isRemActorSyncValue w) . peerListenRemoteActor Dict peer (ChannelID 0) (RemActorId i) -< ()
    emsg <- filterJustE . mapE (fromSyncMessageLast Dict) -< emsgs
    returnA -< event (sa s) id emsg
  SyncApp mf ma -> proc s -> do 
    a <- clientSync ma peer i -< s
    f <- clientSync mf peer i -< s
    returnA -< f a

-- | Perform server side synchronization
serverSync :: (MonadFix m, ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m) 
  => Sync i s a -- ^ Sync strategy
  -> i -- ^ Actor id
  -> GameWire m s a -- ^ Transformer of state
serverSync ms i = case ms of 
  SyncPure a -> pure a
  SyncClient Dict w sa -> onPeers $ \peers -> proc s -> do 
    eas <- sequenceA (listenPeer <$> peers) -< s 
    let ea = F.foldl' mergeR NoEvent eas
    returnA -< event (sa s) id ea
    where
    listenPeer peer = proc _ -> do 
      emsgs <- filterMsgs (isRemActorSyncValue w) . peerListenRemoteActor Dict peer (ChannelID 0) (RemActorId i) -< ()
      filterJustE . mapE (fromSyncMessageLast Dict) -< emsgs
  SyncServer _ _ sa -> arr sa
  SyncApp mf ma -> proc s -> do 
    a <- serverSync ma i -< s
    f <- serverSync mf i -< s
    returnA -< f a