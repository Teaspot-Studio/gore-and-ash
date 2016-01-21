module Game.GoreAndAsh.Sync.Remote.Actor(
    RemoteActor(..)
  , RemActorId(..)
  , RemActorNetMessage(..)
  , clientSync
  , serverSync
  , registerRemoteActor
  ) where

import Control.Monad.Fix 
import Control.Wire
import Control.Wire.Unsafe.Event (event, Event(..))
import Data.Proxy 
import Data.Serialize 
import Data.Word
import GHC.Generics
import Prelude hiding ((.), id)
import qualified Data.ByteString as BS 
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

-- | Need to be called once for each remote actor (remote collections actually do this)
registerRemoteActor :: forall proxy i a m . (ActorMonad m, ActorMessage i, RemoteActor i a) 
  => proxy i -- ^ Proxy of type of base actor id
  -> GameMonadT m () -- ^ Register basic id
registerRemoteActor _ = registerActorTypeRepM (Proxy :: Proxy (RemActorId i))

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
serverFullSync :: (SyncMonad m, ActorMonad m, NetworkMonad m, LoggingMonad m) 
  => Sync m i s a -- ^ Strategy of syncing
  -> Peer -- ^ Which client to send
  -> i -- ^ Id of actor
  -> GameWire m s a
serverFullSync ms peer i = case ms of 
  SyncPure a -> pure a -- Client already knows the constant value
  SyncNone sa -> arr sa
  SyncClient Dict _ w sa -> liftGameMonad1 $ \s -> do
    let val = sa s
    peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage . mkSyncMessage Dict w $! val
    return val
  SyncServer Dict w sa -> liftGameMonad1 $ \s -> do
    let val = sa s
    peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage . mkSyncMessage Dict w $! val
    return val
  SyncCond _ _ ma -> serverFullSync ma peer i
  SyncReject _ _ _ ma -> serverFullSync ma peer i
  SyncApp mf ma -> proc s -> do 
    f <- serverFullSync mf peer i -< s
    a <- serverFullSync ma peer i -< s
    returnA -< f a

-- | Sends all data to remote server
clientFullSync :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m) 
  => Sync m i s a -- ^ Sync strategy
  -> Peer -- ^ Server connection
  -> i -- ^ Actor id
  -> GameWire m s a -- ^ Synchronizing of client state
clientFullSync ms peer i = case ms of 
  SyncPure a -> pure a -- Client already knows the constant value
  SyncNone sa -> arr sa
  SyncClient Dict _ w sa -> liftGameMonad1 $ \s -> do
    let val = sa s
    peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage . mkSyncMessage Dict w $! val
    return val
  SyncServer _ _ sa -> arr sa -- The value is server side
  SyncCond _ _ ma -> clientFullSync ma peer i
  SyncReject _ _ _ ma -> clientFullSync ma peer i
  SyncApp mf ma -> proc s -> do 
    f <- clientFullSync mf peer i -< s
    a <- clientFullSync ma peer i -< s
    returnA -< f a

-- | Perform partial client side synchronization
clientPartialSync :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m) 
  => Sync m i s a -- ^ Sync strategy
  -> Peer -- ^ Server connection
  -> i -- ^ Actor id
  -> GameWire m s a -- ^ Synchronizing of client state
clientPartialSync ms peer i = case ms of 
  SyncPure a -> pure a
  SyncNone sa -> arr sa
  SyncClient Dict _ w sa -> proc s -> do
    emsgs <- filterMsgs (isRemActorSyncValue w) . peerListenRemoteActor Dict peer (ChannelID 0) (RemActorId i) -< ()
    emsg <- filterJustE . mapE (fromSyncMessageLast Dict) -< emsgs
    case emsg of 
      NoEvent -> do 
        let val = event (sa s) id emsg
        liftGameMonad1 (
          peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage 
          . mkSyncMessage Dict w) -< val
        returnA -< val
      Event val -> returnA -< val
  SyncServer Dict w sa -> proc s -> do 
    emsgs <- filterMsgs (isRemActorSyncValue w) . peerListenRemoteActor Dict peer (ChannelID 0) (RemActorId i) -< ()
    emsg <- filterJustE . mapE (fromSyncMessageLast Dict) -< emsgs
    returnA -< event (sa s) id emsg
  SyncCond w sa ma -> proc s -> do 
    e <- w -< s 
    case e of 
      NoEvent -> returnA -< sa s
      Event _ -> clientPartialSync ma peer i -< s 
  SyncReject Dict w wid ma -> proc s -> do 
    a <- clientPartialSync ma peer i -< s 
    e <- w -< (s, a)
    case e of 
      NoEvent -> returnA -< a 
      Event a' -> do
        syncPeer -< a'
        returnA -< a'
    where
    syncPeer = liftGameMonad1 $ do
      peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage 
      . mkSyncMessage Dict wid
  SyncApp mf ma -> proc s -> do 
    f <- clientPartialSync mf peer i -< s
    a <- clientPartialSync ma peer i -< s
    returnA -< f a

-- | Perform partial server side synchronization
serverPartialSync :: (MonadFix m, ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m) 
  => Sync m i s a -- ^ Sync strategy
  -> i -- ^ Actor id
  -> GameWire m s a -- ^ Transformer of state
serverPartialSync ms i = case ms of 
  SyncPure a -> pure a
  SyncNone sa -> arr sa
  SyncClient Dict сpeer w sa -> onPeers $ \peers -> proc s -> do 
    ea <- listenPeer -< s
    let a = sa s 
        a' = event (sa s) id ea
    sequenceA (liftGameMonad1 . syncPeer <$> S.filter (/= сpeer) peers) -< a'
    serverChanged -< (a, a')
    returnA -< a'
    where
    -- Listen field owner about updates
    listenPeer = proc _ -> do 
      emsgs <- filterMsgs (isRemActorSyncValue w) . peerListenRemoteActor Dict сpeer (ChannelID 0) (RemActorId i) -< ()
      filterJustE . mapE (fromSyncMessageLast Dict) -< emsgs
    -- Sync given peer field state
    syncPeer peer = do
      peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage 
      . mkSyncMessage Dict w
    -- Detect when server changes the value and resync owner
    serverChanged = mkSFN $ \(_, a') -> ((), go a')
      where 
      go v = mkGen $ \_ (a, a') -> if v == a 
        then return (Right (), go a')
        else do 
          syncPeer сpeer a
          return (Right (), go a')

  SyncServer Dict w sa -> onPeers $ \peers -> proc s -> do 
    let val = sa s
    sequenceA (syncPeer <$> peers) -< val 
    returnA -< val
    where
    syncPeer peer = liftGameMonad1 $ do
      peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage 
      . mkSyncMessage Dict w
  SyncCond w sa ma -> proc s -> do 
    e <- w -< s 
    case e of 
      NoEvent -> returnA -< sa s
      Event _ -> serverPartialSync ma i -< s 
  SyncReject Dict w wid ma -> proc s -> do 
    a <- serverPartialSync ma i -< s 
    e <- w -< (s, a)
    case e of 
      NoEvent -> returnA -< a 
      Event a' -> do
        onPeers (sequenceA . fmap syncPeer) -< a'
        returnA -< a'
    where
    syncPeer peer = liftGameMonad1 $ do
      peerSendRemoteActorMsg Dict peer (ChannelID 0) (RemActorId i) ReliableMessage 
      . mkSyncMessage Dict wid
  SyncApp mf ma -> proc s -> do 
    f <- serverPartialSync mf i -< s
    a <- serverPartialSync ma i -< s
    returnA -< f a

-- | Perform client side synchronization
clientSync :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, RemoteActor i a) 
  => Sync m i s a -- ^ Sync strategy
  -> Peer -- ^ Server connection
  -> i -- ^ Actor id
  -> GameWire m s a -- ^ Synchronizing of client state
clientSync ms peer i = proc s -> do 
  peerSendIndexed peer (ChannelID 0) (RemActorId i) ReliableMessage . now -< RemActorSyncRequest
  syncOnRequest -< s 
  clientPartialSync ms peer i -< s
  where 
    syncOnRequest = proc s -> do 
      emsgs <- filterMsgs isRemActorSyncRequest . peerIndexedMessages peer (ChannelID 0) (RemActorId i) -< ()
      case emsgs of 
        NoEvent -> returnA -< ()
        Event _ -> pure () . clientFullSync  ms peer i -< s

-- | Perform server side synchronization
serverSync :: (MonadFix m, ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, RemoteActor i a) 
  => Sync m i s a -- ^ Sync strategy
  -> i -- ^ Actor id
  -> GameWire m s a -- ^ Synchronizing of server state
serverSync ms i = proc s -> do 
  syncOnRequest -< s 
  serverPartialSync ms i -< s
  where 
    syncOnRequest = onPeers $ \peers -> sequenceA $ syncOnRequestPeer <$> peers

    syncOnRequestPeer peer = proc s -> do 
      emsgs <- filterMsgs isRemActorSyncRequest . peerIndexedMessages peer (ChannelID 0) (RemActorId i) -< ()
      case emsgs of 
        NoEvent -> returnA -< ()
        Event _ -> pure () . serverFullSync  ms peer i -< s