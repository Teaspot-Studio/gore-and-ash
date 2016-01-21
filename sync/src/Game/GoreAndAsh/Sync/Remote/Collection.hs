module Game.GoreAndAsh.Sync.Remote.Collection(
    RemActorCollId(..)
  , remoteActorCollectionServer
  , remoteActorCollectionClient
  ) where

import Control.DeepSeq
import Control.Monad (liftM)
import Control.Monad.Fix 
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Filterable
import Data.Hashable 
import Data.Proxy 
import Data.Serialize
import GHC.Generics
import Prelude hiding ((.), id)
import qualified Data.Foldable as F 
import qualified Data.Sequence as S 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging 
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync.API
import Game.GoreAndAsh.Sync.Message 

import Game.GoreAndAsh.Sync.Remote.Actor

-- | Unique id space for remote collections actors
newtype RemActorCollId = RemActorCollId { unRemActorCollId :: Int } 
  deriving (Show, Eq, Ord, Generic)

instance NFData RemActorCollId 
instance Hashable RemActorCollId

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

-- | Helper to filter out specific message type
isRemActorCollNetSpawn :: RemActorCollNetMessage -> Bool 
isRemActorCollNetSpawn m = case m of 
  RemActorCollNetSpawn _ -> True 
  _ -> False 

-- | Helper to filter out specific message type
isRemActorCollNetDespawn :: RemActorCollNetMessage -> Bool 
isRemActorCollNetDespawn m = case m of 
  RemActorCollNetDespawn _ -> True 
  _ -> False

-- | Helper to filter out specific message type
isRemActorCollRequestOthers :: RemActorCollNetMessage -> Bool 
isRemActorCollRequestOthers m = case m of 
  RemActorCollRequestOthers -> True 
  _ -> False

-- | Helper that performs monadic action over value of event or returns default value
-- Note: the function is isomorphic to @Data.Maybe.maybe@
onEvent :: Monad m => b -> Event a -> (a -> m b) -> m b
onEvent def e f = case e of 
  NoEvent -> return def 
  Event a -> f a 

-- | Server side collection of network actors that are automatically
-- synchronized to remote clients.
-- 
-- Second wire input is event with new actors to add to the collection
-- Third wire input is event with id of actor to delete from the collection
remoteActorCollectionServer :: forall m a b c c2 i . (MonadFix m, SyncMonad m, LoggingMonad m, ActorMonad m, NetworkMonad m, Eq i, RemoteActor i b, DynCollection c, FilterConstraint c (GameWireIndexed m i a b), FilterConstraint c (Either () (b, i)), F.Foldable c2, Functor c2) 
  => c (GameActor m i a b) -- ^ Initial set of actors
  -> GameActor m RemActorCollId (a, Event (c (GameActor m i a b)), Event (c2 i)) (c b)
remoteActorCollectionServer initialActors = do
  registerRemoteActor (Proxy :: Proxy i)
  makeActor $ \cid -> proc (a, addEvent, remEvent) -> do 
    liftGameMonadOnce (registerActorTypeRepM (Proxy :: Proxy i)) -< ()
    (bs, is) <- dynCollection' cid -< (a, addEvent, remEvent)
    respondRequestOthers cid -< is
    returnA -< bs 
  where 
  -- | If any client asks for other actors, send ids
  respondRequestOthers :: RemActorCollId -> GameWire m (c i) ()
  respondRequestOthers cid = onPeers listenPeers
    where
      listenPeers :: S.Seq Peer -> GameWire m (c i) ()
      listenPeers peers = proc is -> do 
        sequenceA (listenPeer <$> peers) -< is 
        returnA -< ()

      listenPeer :: Peer -> GameWire m (c i) ()
      listenPeer peer = proc is -> do 
        e <- filterMsgs isRemActorCollRequestOthers . peerIndexedMessages peer (ChannelID 0) cid -< ()
        let msgs = RemActorCollNetSpawn . toCounter <$> is
        peerSendIndexedMany peer (ChannelID 0) cid ReliableMessage -< const msgs <$> e
        returnA -< ()

  -- | Modified dynamic collection implementation
  dynCollection' cid = mkGen $ \ds input -> do 
    arrs <- sequence initialActors
    go arrs ds input
    where
    -- | Send ids to clients
    sendSpawnMsgs is = do 
      peers <- networkPeersM 
      F.forM_ peers $ \peer -> F.forM_ is $ \i -> 
        peerSendIndexedM peer (ChannelID 0) cid ReliableMessage $ RemActorCollNetSpawn i

    -- | Send ids to clients
    sendDespawnMsgs is = do 
      peers <- networkPeersM 
      F.forM_ peers $ \peer -> F.forM_ is $ \i -> 
        peerSendIndexedM peer (ChannelID 0) cid ReliableMessage $ RemActorCollNetDespawn i

    -- | Each iteration do
    go :: c (GameWireIndexed m i a b)
      -> GameTime
      -> (a, Event (c (GameActor m i a b)), Event (c2 i))
      -> GameMonadT m (Either () (c b, c i), GameWire m (a, Event (c (GameActor m i a b)), Event (c2 i)) (c b, c i))
    go currentWires ds (a, addEvent, removeEvent) = do
      -- | Adding new wires
      newAddedWires <- onEvent currentWires addEvent $ \newActors -> do 
        addWires <- sequence newActors 
        sendSpawnMsgs $ toCounter . indexedId <$> addWires
        return $ currentWires `concatDynColl` addWires

      -- | Removing wires
      newRemovedWires <- onEvent newAddedWires removeEvent $ \ids ->  do 
        sendDespawnMsgs $ toCounter <$> ids
        return $ F.foldl' (\acc i -> fFilter ((/= i) . indexedId) acc) newAddedWires ids

      -- | Calculating outputs
      (bs, newWiresCntrls) <- liftM unzipDynColl $ mapM (\w -> stepWire w ds (Right a)) $ indexedWire <$> newRemovedWires
      let newWires = uncurry updateIndexedWire <$> (fmap const newWiresCntrls `zipDynColl` newRemovedWires)

      -- | Attach ids
      let is = indexedId <$> newRemovedWires :: c i
      let bs' = (\(eb, i) -> (, i) <$> eb) <$> bs `zipDynColl` is :: c (Either () (b, i))
      let bs'' = unzipDynColl $ rightsDynColl bs' :: (c b, c i)
      return $ length newWires `seq` length is `seq` (Right bs'', mkGen $ go newWires)


-- | Client side collection of network actors that are automatically
-- synchronized to remote clients.
remoteActorCollectionClient :: forall m i a b . (SyncMonad m, LoggingMonad m, ActorMonad m, NetworkMonad m, Eq i, RemoteActor i b) 
  => RemActorCollId -- ^ Corresponding server collection id
  -> Peer -- ^ Server peer
  -> (i -> GameActor m i a b) -- ^ How to construct client side actors
  -> GameActor m RemActorCollId a (S.Seq b)
remoteActorCollectionClient cid peer mkActor = do 
  registerRemoteActor (Proxy :: Proxy i)
  makeFixedActor cid $ proc a -> do
    peerSendIndexed peer (ChannelID 0) cid ReliableMessage . now -< RemActorCollRequestOthers
    emsgs <- peerIndexedMessages peer (ChannelID 0) cid -< ()
    addEvent <- mapE (fmap $ \(RemActorCollNetSpawn i) -> fromCounter i) . filterMsgs isRemActorCollNetSpawn -< emsgs
    remEvent <- mapE (fmap $ \(RemActorCollNetDespawn i) -> fromCounter i) . filterMsgs isRemActorCollNetDespawn -< emsgs
    dynCollection emptyDynColl -< (a, fmap mkActor <$> addEvent, remEvent)