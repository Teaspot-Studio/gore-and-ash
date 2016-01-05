module Game.GoreAndAsh.Sync.Message(
    NetworkMessage(..)
  -- | Getting messages
  , peerIndexedMessages
  , peerProcessIndexed
  , peerProcessIndexedM
  -- | Sending messages
  , peerSendIndexedM
  , peerSendIndexed
  , peerSendIndexedDyn
  , peerSendIndexedMany
  , peerSendIndexedManyDyn
  -- | Helpers for actors
  , netStateActor
  , netStateActorM
  , netStateActorFixed
  , netStateActorFixedM
  -- | Helpers
  , filterMsgs
  ) where

import Control.Monad.Fix 
import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Maybe 
import Data.Serialize
import Data.Typeable 
import Data.Word
import Prelude hiding ((.), id)
import qualified Data.ByteString as BS 
import qualified Data.Foldable as F 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network

-- | Extension for actor message, messages that are sent to remote host
class ActorMessage i => NetworkMessage i where
  -- | Corresponding message payload for @i@ identifier, usually ADT
  type NetworkMessageType i :: *

-- | Fires when network messages for specific actor has arrived
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerIndexedMessages :: forall m i a . (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we are listening
  -> ChannelID -- ^ Which channel we are listening
  -> i -- ^ ID of actor 
  -> GameWire m a (Event [NetworkMessageType i]) -- ^ Messages that are addressed to the actor
peerIndexedMessages p chid i = filterE (not . null) 
  . mapE (catMaybes . fmap parse . filter hasId)
  . peerMessages p chid
  where
    parse :: BS.ByteString -> Maybe (NetworkMessageType i)
    parse bs = case decode bs of 
      Left _ -> Nothing
      Right (_ :: Word64, mbs :: BS.ByteString) -> case decode mbs of 
        Left _ -> Nothing 
        Right m -> Just m

    hasId :: BS.ByteString -> Bool
    hasId bs = case decode bs of
      Left _ -> False 
      Right (w64 :: Word64, _ :: BS.ByteString) -> fromIntegral w64 == toCounter i

-- | Encodes a message for specific actor type and send it to remote host
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerSendIndexedM :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we sending to
  -> ChannelID -- ^ Which channel we are sending within
  -> i -- ^ ID of actor
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> NetworkMessageType i -- ^ Message to send
  -> m ()
peerSendIndexedM p chid i mt msg = do 
  let w64 = fromIntegral (toCounter i) :: Word64
      msg' = Message mt $ encode (w64, encode msg)
  peerSendM p chid msg'

-- | Encodes a message for specific actor type and send it to remote host, arrow version
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerSendIndexed :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we sending to
  -> ChannelID -- ^ Which channel we are sending within
  -> i -- ^ ID of actor
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event (NetworkMessageType i)) (Event ())
peerSendIndexed p chid i mt = liftGameMonadEvent1 $ peerSendIndexedM p chid i mt

-- | Encodes a message for specific actor type and send it to remote host, arrow version.
-- Takes peer, id and message as arrow input.
peerSendIndexedDyn :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => ChannelID -- ^ Which channel we are sending within
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event (Peer, i, NetworkMessageType i)) (Event ())
peerSendIndexedDyn chid mt = liftGameMonadEvent1 $ \(p, i, msg) -> peerSendIndexedM p chid i mt msg

-- | Encodes a message for specific actor type and send it to remote host, arrow version
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerSendIndexedMany :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i), F.Foldable t)
  => Peer -- ^ Which peer we sending to
  -> ChannelID -- ^ Which channel we are sending within
  -> i -- ^ ID of actor
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event (t (NetworkMessageType i))) (Event ())
peerSendIndexedMany p chid i mt = liftGameMonadEvent1 . F.mapM_ $ peerSendIndexedM p chid i mt

-- | Encodes a message for specific actor type and send it to remote host, arrow version.
-- Takes peer, id and message as arrow input.
peerSendIndexedManyDyn :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => ChannelID -- ^ Which channel we are sending within
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event [(Peer, i, NetworkMessageType i)]) (Event ())
peerSendIndexedManyDyn chid mt = liftGameMonadEvent1 . F.mapM_ $ \(p, i, msg) -> peerSendIndexedM p chid i mt msg


-- | Same as @peerIndexedMessages@, but transforms input state with given handler
peerProcessIndexed :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we are listening
  -> ChannelID -- ^ Which channel we are listening
  -> i -- ^ ID of actor
  -> (a -> NetworkMessageType i -> a) -- ^ Handler of message
  -> GameWire m a a -- ^ Updates @a@ with given handler for messages
peerProcessIndexed p chid i f = proc a -> do 
  emsgs <- peerIndexedMessages p chid i -< ()
  returnA -< event a (F.foldl' f a) emsgs

-- | Same as @peerIndexedMessages@, but transforms input state with given handler, monadic version
peerProcessIndexedM :: (NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we are listening
  -> ChannelID -- ^ Which channel we are listening
  -> i -- ^ ID of actor
  -> (a -> NetworkMessageType i -> GameMonadT m a) -- ^ Handler of message
  -> GameWire m a a -- ^ Updates @a@ with given handler for messages
peerProcessIndexedM p chid i f = proc a -> do 
  emsgs <- peerIndexedMessages p chid i -< ()
  liftGameMonad2 (\emsgs a -> case emsgs of
    NoEvent -> return a 
    Event msgs -> F.foldlM f a msgs) -< (emsgs, a) 

-- | Helper to create stateful actors, same as @stateActor@
netStateActor :: (ActorMonad m, NetworkMonad m, LoggingMonad m, MonadFix m, NetworkMessage i, Typeable (ActorMessageType i), Serialize (NetworkMessageType i))
  => (i -> b) -- ^ Inital value of state
  -> (i -> b -> ActorMessageType i -> b) -- ^ Handler for messages
  -> (b -> Peer) -- ^ Way to get peer value from inital getter
  -> Word8 -- ^ Channels count to listen
  -> (i -> ChannelID -> b -> NetworkMessageType i -> b) -- ^ Handler for network messages
  -> (i -> GameWire m (a, b) b) -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
netStateActor bi f getPeer channels fn w = stateActor bi f $ \i -> proc (a, b) -> do 
  b' <- chainWires ((\ch -> peerProcessIndexed (getPeer $ bi i) ch i (fn i ch)) . ChannelID <$> [0 .. channels-1]) -< b
  w i -< (a, b')

-- | Helper to create stateful actors, same as @stateActor@
netStateActorM :: (ActorMonad m, NetworkMonad m, LoggingMonad m, MonadFix m, NetworkMessage i, Typeable (ActorMessageType i), Serialize (NetworkMessageType i))
  => (i -> b) -- ^ Inital value of state
  -> (i -> b -> ActorMessageType i -> GameMonadT m b) -- ^ Handler for messages
  -> (b -> Peer) -- ^ Way to get peer value from inital getter
  -> Word8 -- ^ Channels count to listen
  -> (i -> ChannelID -> b -> NetworkMessageType i -> GameMonadT m b) -- ^ Handler for network messages
  -> (i -> GameWire m (a, b) b) -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
netStateActorM bi f getPeer channels fn w = stateActorM bi f $ \i -> proc (a, b) -> do 
  b' <- chainWires ((\ch -> peerProcessIndexedM (getPeer $ bi i) ch i (fn i ch)) . ChannelID <$> [0 .. channels-1]) -< b
  w i -< (a, b')

-- | Helper to create stateful actors with specific id, same as @stateActor@
netStateActorFixed :: (ActorMonad m, NetworkMonad m, LoggingMonad m, MonadFix m, NetworkMessage i, Typeable (ActorMessageType i), Serialize (NetworkMessageType i))
  => i -- ^ Fixed id of actor
  -> b -- ^ Inital value of state
  -> (b -> ActorMessageType i -> b) -- ^ Handler for messages
  -> Peer -- ^ Peer to use for listening
  -> Word8 -- ^ Channels count to listen
  -> (ChannelID -> b -> NetworkMessageType i -> b) -- ^ Handler for network messages
  -> GameWire m (a, b) b -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
netStateActorFixed i bi f peer channels fn w = stateActorFixed i bi f $ proc (a, b) -> do 
  b' <- chainWires ((\ch -> peerProcessIndexed peer ch i (fn ch)) . ChannelID <$> [0 .. channels-1]) -< b
  w -< (a, b')

-- | Helper to create stateful actors with specific id, same as @stateActorM@
netStateActorFixedM :: (ActorMonad m, NetworkMonad m, LoggingMonad m, MonadFix m, NetworkMessage i, Typeable (ActorMessageType i), Serialize (NetworkMessageType i))
  => i -- ^ Fixed id of actor
  -> b -- ^ Inital value of state
  -> (b -> ActorMessageType i -> GameMonadT m b) -- ^ Handler for messages
  -> Peer -- ^ Peer to use for listening
  -> Word8 -- ^ Channels count to listen
  -> (ChannelID -> b -> NetworkMessageType i -> GameMonadT m b) -- ^ Handler for network messages
  -> GameWire m (a, b) b -- ^ Handler that transforms current state
  -> GameActor m i a b -- ^ Resulting actor incapsulating @b@ in itself
netStateActorFixedM i bi f peer channels fn w = stateActorFixedM i bi f $ proc (a, b) -> do 
  b' <- chainWires ((\ch -> peerProcessIndexedM peer ch i (fn ch)) . ChannelID <$> [0 .. channels-1]) -< b
  w -< (a, b')

-- | Helper to filter output of @peerIndexedMessages@
filterMsgs :: Monad m
  => (a -> Bool) -- ^ Predicate to test message
  -> GameWire m (Event [a]) (Event [a])
filterMsgs p = mapE (filter p)