{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  -- | Helpers
  , filterMsgs
  ) where

import Control.Wire
import Control.Wire.Unsafe.Event
import Data.Maybe 
import Data.Serialize
import Data.Typeable 
import Data.Word
import Prelude hiding ((.), id)
import qualified Control.Monad as M 
import qualified Data.ByteString as BS 
import qualified Data.Foldable as F 
import qualified Data.Sequence as S 

import Game.GoreAndAsh
import Game.GoreAndAsh.Actor
import Game.GoreAndAsh.Logging
import Game.GoreAndAsh.Network
import Game.GoreAndAsh.Sync.API 
import Game.GoreAndAsh.Sync.State

-- | Fires when network messages for specific actor has arrived
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerIndexedMessages :: forall m i a . (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we are listening
  -> ChannelID -- ^ Which channel we are listening
  -> i -- ^ ID of actor 
  -> GameWire m a (Event (S.Seq (NetworkMessageType i))) -- ^ Messages that are addressed to the actor
peerIndexedMessages p chid i = inhibit2NoEvent $ proc _ -> do 
  netid <- mkNetId -< ()
  emsgs <- peerMessages p chid -< ()
  filterE (not . S.null) . mapE (\(netid, msgs) -> catMaybesSeq $ (parse netid) <$> msgs)
    -< (netid, ) <$> emsgs
  where
    inhibit2NoEvent w = w <|> never 

    -- | If actor is new, register actor and cache the id
    mkNetId :: GameWire m () Word64
    mkNetId = go False
      where
      go sended = mkGen $ \_ _ -> do 
        let !tr = actorFingerprint (Proxy :: Proxy i)
        mnid <- getSyncIdM tr
        case mnid of 
          Nothing -> do 
            r <- syncGetRoleM
            case r of 
              SyncMaster -> do 
                !nid <- registerSyncIdM tr
                return (Right nid, pure nid)
              SyncSlave -> do 
                M.unless sended $ syncRequestIdM p (Proxy :: Proxy i)
                return (Left (), go True)
          Just !nid -> return (Right nid, pure nid)

    -- | Parses packet, decodes only user messages
    parse :: Word64 -> BS.ByteString -> Maybe (NetworkMessageType i)
    parse !netid !bs = case decode bs of 
      Left _ -> Nothing
      Right (fp :: Word64, bs2 :: BS.ByteString) -> if fp == 0 
        then Nothing
        else case decode bs2 of 
          Left _ -> Nothing
          Right (w64 :: Word64, mbs :: BS.ByteString) -> if not (fp == netid && fromIntegral w64 == toCounter i)
            then Nothing
            else case decode mbs of 
              Left _ -> Nothing 
              Right !m -> Just $! m

-- | catMaybes for sequences
catMaybesSeq :: S.Seq (Maybe a) -> S.Seq a 
catMaybesSeq = fmap fromJust . S.filter isJust

-- | Encodes a message for specific actor type and send it to remote host
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerSendIndexedM :: forall m i . (SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we sending to
  -> ChannelID -- ^ Which channel we are sending within
  -> i -- ^ ID of actor
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> NetworkMessageType i -- ^ Message to send
  -> m ()
peerSendIndexedM p chid i mt msg = do 
  mnid <- getSyncIdM $ actorFingerprint (Proxy :: Proxy i)
  case mnid of 
    Nothing -> syncScheduleMessageM p chid i mt msg  -- schedule message send when we resolve sync id 
    Just nid -> do 
      let w64 = fromIntegral (toCounter i) :: Word64
          msg' = Message mt $! encode (nid, encode (w64, encode msg))
      peerSendM p chid msg'

-- | Encodes a message for specific actor type and send it to remote host, arrow version
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerSendIndexed :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we sending to
  -> ChannelID -- ^ Which channel we are sending within
  -> i -- ^ ID of actor
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event (NetworkMessageType i)) (Event ())
peerSendIndexed p chid i mt = liftGameMonadEvent1 $ peerSendIndexedM p chid i mt

-- | Encodes a message for specific actor type and send it to remote host, arrow version.
-- Takes peer, id and message as arrow input.
peerSendIndexedDyn :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => ChannelID -- ^ Which channel we are sending within
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event (Peer, i, NetworkMessageType i)) (Event ())
peerSendIndexedDyn chid mt = liftGameMonadEvent1 $ \(p, i, msg) -> peerSendIndexedM p chid i mt msg

-- | Encodes a message for specific actor type and send it to remote host, arrow version
-- Note: mid-level API is not safe to use with low-level at same time as
-- first bytes of formed message are used for actor id. So, you need to 
-- have a special forbidden id for you custom messages.
peerSendIndexedMany :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i), F.Foldable t)
  => Peer -- ^ Which peer we sending to
  -> ChannelID -- ^ Which channel we are sending within
  -> i -- ^ ID of actor
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event (t (NetworkMessageType i))) (Event ())
peerSendIndexedMany p chid i mt = liftGameMonadEvent1 . F.mapM_ $ peerSendIndexedM p chid i mt

-- | Encodes a message for specific actor type and send it to remote host, arrow version.
-- Takes peer, id and message as arrow input.
peerSendIndexedManyDyn :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i), F.Foldable t)
  => ChannelID -- ^ Which channel we are sending within
  -> MessageType -- ^ Strategy of the message (reliable, unordered etc.)
  -> GameWire m (Event (t (Peer, i, NetworkMessageType i))) (Event ())
peerSendIndexedManyDyn chid mt = liftGameMonadEvent1 . F.mapM_ $ \(p, i, msg) -> peerSendIndexedM p chid i mt msg


-- | Same as @peerIndexedMessages@, but transforms input state with given handler
peerProcessIndexed :: (ActorMonad m,SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
  => Peer -- ^ Which peer we are listening
  -> ChannelID -- ^ Which channel we are listening
  -> i -- ^ ID of actor
  -> (a -> NetworkMessageType i -> a) -- ^ Handler of message
  -> GameWire m a a -- ^ Updates @a@ with given handler for messages
peerProcessIndexed p chid i f = proc a -> do 
  emsgs <- peerIndexedMessages p chid i -< ()
  returnA -< event a (F.foldl' f a) emsgs

-- | Same as @peerIndexedMessages@, but transforms input state with given handler, monadic version
peerProcessIndexedM :: (ActorMonad m, SyncMonad m, NetworkMonad m, LoggingMonad m, NetworkMessage i, Serialize (NetworkMessageType i))
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

-- | Helper to filter output of @peerIndexedMessages@
filterMsgs :: (Monad m)
  => (a -> Bool) -- ^ Predicate to test message
  -> GameWire m (Event (S.Seq a)) (Event (S.Seq a))
filterMsgs p = filterE (not . S.null) . mapE (S.filter p)